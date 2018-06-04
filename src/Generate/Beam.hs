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
      undefined


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
            do  tmp <- freshStackAllocation
                return $ Value
                  [ I.call 0 label
                  , I.move returnRegister tmp
                  ]
                  (Beam.toSource tmp)

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
      do  tmp <- freshStackAllocation
          Value leftOps leftResult <- fromExpr left
          Value rightOps rightResult <- fromExpr right
          let ops =
                leftOps ++ rightOps ++
                  [ opBif operator leftResult rightResult tmp
                  ]
          return $ Value ops (Beam.toSource tmp)

    Opt.Binop _ _ _ ->
      undefined

    Opt.Function _ _ ->
      undefined

    Opt.Call (Opt.Var variable) args ->
      withReference variable $ \reference ->
        case reference of
          Arg _ ->
            undefined

          Stack _ ->
            undefined

          TopLevel label ->
            do  tmp <- freshStackAllocation
                values <- mapM fromExpr args
                let moveArg value i = I.move (_result value) (Beam.X i)
                    ops = concatMap _ops values ++
                      zipWith moveArg values [0..] ++
                      [ I.call (length args) label
                      , I.move returnRegister tmp
                      ]
                return $ Value ops (Beam.toSource tmp)

    Opt.Call _ _ ->
      undefined

    Opt.TailCall _ _ _ ->
      undefined

    Opt.If _ _ ->
      undefined

    Opt.Let _ _ ->
      undefined

    Opt.Case _ _ _ ->
      undefined

    Opt.Data constructor args ->
      fromData constructor =<< mapM fromExpr args


    Opt.DataAccess _ _ ->
      undefined

    Opt.Access record field ->
      do  fieldReg <- freshStackAllocation
          valueReg <- freshStackAllocation
          loopOnFail <- freshLabel
          Value recordOps recordResult <- fromExpr record
          let getOps =
                [ I.label loopOnFail
                , I.move (Text.pack field) fieldReg
                , I.get_map_elements loopOnFail recordResult
                    [ ( Beam.toRegister fieldReg, Beam.toRegister valueReg )
                    ]
                ]
          return $ Value (recordOps ++ getOps) (Beam.toSource valueReg)

    Opt.Update _ _ ->
      undefined

    Opt.Record fields ->
      do  tmp <- freshStackAllocation
          values <- mapM (fromExpr . snd) fields
          let ops = concatMap _ops values ++
                [ I.put_map_assoc cannotFail (Beam.Map []) tmp $
                    zipWith (toMapPair . fst) fields values
                ]
          return $ Value ops (Beam.toSource tmp)

    Opt.Cmd _ ->
      undefined

    Opt.Sub _ ->
      undefined

    Opt.OutgoingPort _ _ ->
      undefined

    Opt.IncomingPort _ _ ->
      undefined

    Opt.Program _ _ ->
      undefined

    Opt.GLShader _ _ _ ->
      error "WebGL shaders are not supported for server programs"

    Opt.Crash _ _ _ ->
      undefined


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

    Literal.Boolean True ->
      Beam.toSource ("true" :: Text)

    Literal.Boolean False ->
      Beam.toSource ("false" :: Text)


fromData :: String -> [ Value ] -> Gen Value
fromData constructor values =
  do  tmp <- freshStackAllocation
      let ops = concatMap _ops values ++
            I.test_heap (length values + 2) 0
              : I.put_tuple (length values + 1) tmp
              : I.put (Beam.Atom (Text.pack constructor))
              : map (I.put . _result) values
      return $ Value ops (Beam.toSource tmp)


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
  State.modify $ \env -> env
    { _references =
        Map.insert (Var.topLevel moduleName name) (TopLevel label) (_references env)
    }


saveArg :: String -> Beam.X -> Gen ()
saveArg name register =
  State.modify $ \env -> env
    { _references =
        Map.insert (Var.local name) (Arg register) (_references env)
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
