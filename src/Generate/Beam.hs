{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import Codec.Beam.Bifs
import Data.Monoid ()
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as Literal
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Package


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  let
    defs =
      Module.program info

    env =
      Env 1 0 Map.empty
  in
  Beam.encode "pine"
    [ Beam.insertModuleInfo
    , Beam.export "init" 1
    , Beam.export "handle_call" 3
    , Beam.export "handle_cast" 2
    , Beam.export "handle_info" 2
    , Beam.export "terminate" 2
    , Beam.export "code_change" 3
    ] $ State.evalState (concat <$> mapM (fromDef moduleName) defs) env


type Gen a =
  State.State Env a

data Env = Env
  { _nextLabel :: Int
  , _nextStackAllocation :: Int
  , _references :: Map.Map Var.Canonical Reference
  }

data Value = Value
  { _ops :: [Beam.Op]
  , _result :: Beam.Source
  }


data Reference
  = Arg Beam.X
  | Stack Beam.Y
  | TopLevel Beam.Label


fromDef :: ModuleName.Canonical -> Opt.Def -> Gen [Beam.Op]
fromDef moduleName def =
  case def of
    Opt.Def _ "main" (Opt.Program _ (Opt.Call _ [ Opt.Record fields ])) ->
      let
        Just init_ = lookup "init" fields
        Just handleCall = lookup "handleCall" fields
      in
      (++)
        <$> fromBody moduleName id "init" [ "_" ] (Opt.Data "ok" [ init_ ])
        <*> do  let body = Opt.Call handleCall [ Opt.Var (Var.local "state") ]
                fromBody moduleName id "handle_call" [ "_", "_", "state" ]
                  (Opt.Data "reply" [ body, body ])

    Opt.Def _ name (Opt.Function args expr) ->
      fromBody moduleName (namespace moduleName) name args expr

    Opt.Def _ name expr ->
      fromBody moduleName (namespace moduleName) name [] expr

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail definition"


fromBody
  :: ModuleName.Canonical
  -> (String -> String)
  -> String
  -> [String]
  -> Opt.Expr
  -> Gen [Beam.Op]
fromBody moduleName prefix name args body =
  do  pre <- freshLabel
      post <- freshLabel
      resetStackAllocation
      saveTopLevel moduleName name post
      sequence_ $ zipWith saveArg args $ map Beam.X [0..]
      Value bodyOps result <- fromExpr body
      stackNeeded <- getStackAllocations
      return $ concat
        [ [ I.label pre
          , I.func_info (Text.pack (prefix name)) (length args)
          , I.label post
          , I.allocate stackNeeded (length args)
          ]
        , bodyOps
        , [ I.move result returnRegister
          , I.deallocate stackNeeded
          , I.return'
          ]
        ]


fromExpr :: Opt.Expr -> Gen Value
fromExpr expr =
  case expr of
    Opt.Literal literal ->
      return $ Value [] (fromLiteral literal)

    Opt.Var variable ->
      withReference variable $ \reference ->
        case reference of
          Arg x ->
            return $ Value [] (Beam.toSource x)

          Stack y ->
            return $ Value [] (Beam.toSource y)

          TopLevel label ->
            do  dest <- freshStackAllocation
                return $ Value
                  [ I.call 0 label
                  , I.move returnRegister dest
                  ]
                  (Beam.toSource dest)

    Opt.ExplicitList exprs ->
      let
        fromList [] =
          fromData "[]" []
        fromList (first : rest) =
          do  firstValue <- fromExpr first
              restValue  <- fromList rest
              fromData "::" [ firstValue, restValue ]
      in fromList exprs

    Opt.Binop operator left right | inCore operator ->
      do  dest <- freshStackAllocation
          Value leftOps leftResult <- fromExpr left
          Value rightOps rightResult <- fromExpr right
          let ops = leftOps ++ rightOps ++
                [ opBif operator leftResult rightResult dest
                ]
          return $ Value ops (Beam.toSource dest)

    Opt.Binop variable left right -> 
      withReference variable $ \reference ->
        case reference of
          Arg _          -> error "operators are only allowed at top-level"
          Stack _        -> error "operators are only allowed at top-level"
          TopLevel label -> fromFunctionCall label [ left, right ]

    Opt.Function _ _ ->
      error "TODO: function"

    Opt.Call (Opt.Var variable) args ->
      withReference variable $ \reference ->
        case reference of
          Arg _          -> error "TODO: argument functinos"
          Stack _        -> error "TODO: local functions"
          TopLevel label -> fromFunctionCall label args

    Opt.Call _ _ ->
      error "TODO: call"

    Opt.TailCall _ _ _ ->
      error "TODO: tail call"

    Opt.If branches elseExpr ->
      do  elseValue <- fromExpr elseExpr
          finalJump <- freshLabel
          dest      <- freshStackAllocation
          let elseOps = _ops elseValue ++
                [ I.move (_result elseValue) dest
                , I.label finalJump
                ]
          Value
            <$> foldr (fromBranch finalJump dest) (return elseOps) branches
            <*> return (Beam.toSource dest)

    Opt.Let locals body ->
      do  preOps <- mapM fromLetDef locals
          Value ops result <- fromExpr body
          return $ Value (concat preOps ++ ops) result

    Opt.Case _ decider _ ->
      fromDecider decider

    Opt.Data constructor args ->
      fromData constructor =<< mapM fromExpr args


    Opt.DataAccess data_ index ->
      do  dest <- freshStackAllocation
          Value ops result <- fromExpr data_
          let get =
                [ I.move result dest
                , I.get_tuple_element dest (index + 1) dest
                ]
          return $ Value (ops ++ get) (Beam.toSource dest)

    Opt.Access record field ->
      do  dest <- freshStackAllocation
          loopOnFail <- freshLabel
          Value recordOps recordResult <- fromExpr record
          let getOps =
                [ I.label loopOnFail
                , I.move (Text.pack field) dest
                , I.get_map_elements loopOnFail recordResult
                    [ ( Beam.toRegister dest, Beam.toRegister dest )
                    ]
                ]
          return $ Value (recordOps ++ getOps) (Beam.toSource dest)

    Opt.Update _ _ ->
      error "TODO: update"

    Opt.Record fields ->
      do  dest <- freshStackAllocation
          values <- mapM (fromExpr . snd) fields
          let ops = concatMap _ops values ++
                [ I.put_map_assoc cannotFail (Beam.Map []) dest $
                    zipWith (toMapPair . fst) fields values
                ]
          return $ Value ops (Beam.toSource dest)

    Opt.Cmd _ ->
      error "TODO: Cmd"

    Opt.Sub _ ->
      error "TODO: Sub"

    Opt.OutgoingPort _ _ ->
      error "TODO: outgoig port"

    Opt.IncomingPort _ _ ->
      error "TODO: incoming port"

    Opt.Program _ _ ->
      error "TODO: program"

    Opt.GLShader _ _ _ ->
      error "shaders are not supported for server programs"

    Opt.Crash _ _ _ ->
      error "TODO: crash"


fromLiteral :: Literal.Literal -> Beam.Source
fromLiteral literal =
  case literal of
    Literal.Chr char ->
      Beam.toSource (Char.ord char)

    Literal.Str string ->
      Beam.toSource $ Beam.Binary (lazyBytes string)

    Literal.IntNum number ->
      Beam.toSource number

    Literal.FloatNum number ->
      Beam.toSource (Beam.Float number)

    Literal.Boolean bool ->
      if bool then true else false


fromData :: String -> [ Value ] -> Gen Value
fromData constructor values =
  do  dest <- freshStackAllocation
      let ops = concatMap _ops values ++
            I.test_heap (length values + 2) 0
              : I.put_tuple (length values + 1) dest
              : I.put (Beam.Atom (Text.pack constructor))
              : map (I.put . _result) values
      return $ Value ops (Beam.toSource dest)


fromFunctionCall :: Beam.Label -> [ Opt.Expr ] -> Gen Value
fromFunctionCall label args =
  do  dest <- freshStackAllocation
      values <- mapM fromExpr args
      let moveArg value i = I.move (_result value) (Beam.X i)
          ops = concatMap _ops values ++
            zipWith moveArg values [0..] ++
            [ I.call (length args) label
            , I.move returnRegister dest
            ]
      return $ Value ops (Beam.toSource dest)


fromLetDef :: Opt.Def -> Gen [ Beam.Op ]
fromLetDef def =
  case def of
    Opt.Def _ name expr ->
      do  dest <- freshStackAllocation
          Value ops result <- fromExpr expr
          saveStack name dest
          return $ ops ++ [ I.move result dest ]
          

    Opt.TailDef _ _ _ _ ->
      undefined


fromDecider :: Opt.Decider Opt.Choice -> Gen Value
fromDecider decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      fromExpr expr

    Opt.Leaf (Opt.Jump _) ->
      error "TODO: leaf"

    Opt.Chain _ _ _ ->
      error "TODO: chain"

    Opt.FanOut _ _ _ ->
      error "TODO: fan out"


fromBranch :: Beam.Label -> Beam.Y -> ( Opt.Expr, Opt.Expr ) -> Gen [ Beam.Op ] -> Gen [ Beam.Op ]
fromBranch finalJump dest ( condExpr, ifExpr ) elseOps =
  do  condValue <- fromExpr condExpr
      ifValue   <- fromExpr ifExpr
      elseJump  <- freshLabel
      let ops = concat
            [ _ops condValue
            , [ I.is_eq elseJump (_result condValue) true ]
            , _ops ifValue
            , [ I.move (_result ifValue) dest
              , I.jump finalJump
              , I.label elseJump
              ]
            ]
      fmap (ops ++) elseOps


toMapPair :: String -> Value -> ( Beam.Source, Beam.Source )
toMapPair key (Value _ value) =
  ( Beam.toSource $ Beam.Atom (Text.pack key), value )


opBif :: Var.Canonical -> Beam.Source -> Beam.Source -> Beam.Y -> Beam.Op
opBif (Var.Canonical _ "+")  = I.bif2 noFailure Erlang'splus_2
opBif (Var.Canonical _ "-")  = I.bif2 noFailure Erlang'sminus_2
opBif (Var.Canonical _ "*")  = I.bif2 noFailure Erlang'stimes_2
opBif (Var.Canonical _ "/")  = I.bif2 noFailure Erlang'div_2
opBif (Var.Canonical _ ">")  = I.bif2 noFailure Erlang'sgt_2
opBif (Var.Canonical _ ">=") = I.bif2 noFailure Erlang'sge_2
opBif (Var.Canonical _ "<")  = I.bif2 noFailure Erlang'slt_2
opBif (Var.Canonical _ "<=") = I.bif2 noFailure Erlang'sle_2
opBif (Var.Canonical _ "==") = I.bif2 noFailure Erlang'seqeq_2
opBif (Var.Canonical _ "/=") = I.bif2 noFailure Erlang'sneqeq_2
opBif (Var.Canonical _ "^")  = I.bif2 noFailure Math'pow
opBif (Var.Canonical _ "%")  = I.bif2 noFailure Erlang'rem
opBif (Var.Canonical _ "//") = I.bif2 noFailure Erlang'rem
opBif (Var.Canonical _ "&&") = I.bif2 noFailure Erlang'band
opBif (Var.Canonical _ "||") = I.bif2 noFailure Erlang'bor
opBif (Var.Canonical _ "++") = I.bif2 noFailure Erlang'ebif_plusplus_2
opBif var = error $ "binary operator (" ++ Var.toString var ++ ") is not handled"


true :: Beam.Source
true =
  Beam.toSource ("true" :: Text)


false :: Beam.Source
false =
  Beam.toSource ("false" :: Text)


noFailure :: Beam.Label
noFailure =
  Beam.Label 0


inCore :: Var.Canonical -> Bool
inCore (Var.Canonical home _) =
  case home of
    Var.Module (ModuleName.Canonical pkg _) | Package.core == pkg ->
      True

    _ ->
      False


namespace :: ModuleName.Canonical -> String -> String
namespace (ModuleName.Canonical package name) var =
  Package.toString package
    ++ "/" ++ ModuleName.toString name
    ++ "#" ++ var


lazyBytes :: String -> LazyBytes.ByteString
lazyBytes =
  encodeUtf8 . LazyText.fromStrict . Text.pack



-- ENVIRONMENT


saveTopLevel :: ModuleName.Canonical -> String -> Beam.Label -> Gen ()
saveTopLevel moduleName name label =
  save (Var.topLevel moduleName name) (TopLevel label)


saveArg :: String -> Beam.X -> Gen ()
saveArg name register =
  save (Var.local name) (Arg register)


saveStack :: String -> Beam.Y -> Gen ()
saveStack name register =
  save (Var.local name) (Stack register)


save :: Var.Canonical -> Reference -> Gen ()
save var reference =
  State.modify $ \env -> env
    { _references = Map.insert var reference (_references env)
    }


freshLabel :: Gen Beam.Label
freshLabel =
  do  i <- State.gets _nextLabel
      State.modify $ \env -> env { _nextLabel = i + 1 }
      return $ Beam.Label i


freshStackAllocation :: Gen Beam.Y
freshStackAllocation =
  do  i <- State.gets _nextStackAllocation
      State.modify $ \env -> env { _nextStackAllocation = i + 1 }
      return $ Beam.Y i


resetStackAllocation :: Gen ()
resetStackAllocation =
  State.modify $ \env -> env { _nextStackAllocation = 0 }


getStackAllocations :: Gen Int
getStackAllocations =
  State.gets _nextStackAllocation


withReference :: Var.Canonical -> (Reference -> Gen a) -> Gen a
withReference variable f =
  do  maybeReference <- Map.lookup variable <$> State.gets _references
      case maybeReference of
        Just reference ->
          f reference

        Nothing ->
          error $ "VARIABLE `" ++ Var.name variable ++ "` is unbound"


returnRegister :: Beam.X
returnRegister =
  Beam.X 0


cannotFail :: Beam.Label
cannotFail =
  Beam.Label 0
