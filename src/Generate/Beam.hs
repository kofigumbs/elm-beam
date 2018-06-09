{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import Codec.Beam.Bifs
import Data.Monoid ()
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
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
import qualified Generate.Environment as Env


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  let
    defs = Module.program info
  in
  Beam.encode "pine"
    [ Beam.insertModuleInfo
    , Beam.export "init" 1
    , Beam.export "handle_call" 3
    , Beam.export "handle_cast" 2
    , Beam.export "handle_info" 2
    , Beam.export "terminate" 2
    , Beam.export "code_change" 3
    ] $ Env.run $ concat <$> mapM (fromDef moduleName) defs


fromDef :: ModuleName.Canonical -> Opt.Def -> Env.Gen [Beam.Op]
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
  -> Env.Gen [Beam.Op]
fromBody moduleName prefix name args body =
  do  Env.resetStackAllocation
      pre <- Env.freshLabel
      post <- Env.registerTopLevel moduleName name
      argOps <- mapM Env.registerArgument args
      Env.Value bodyOps result <- fromExpr body
      stackNeeded <- Env.getStackAllocations
      return $ concat
        [ [ I.label pre
          , I.func_info (Text.pack (prefix name)) (length args)
          , I.label post
          , I.allocate stackNeeded (length args)
          ]
        , argOps
        , bodyOps
        , [ I.move result Env.returnRegister
          , I.deallocate stackNeeded
          , I.return'
          ]
        ]


fromExpr :: Opt.Expr -> Env.Gen Env.Value
fromExpr expr =
  case expr of
    Opt.Literal literal ->
      return $ Env.Value [] (fromLiteral literal)

    Opt.Var variable ->
      Env.withReference variable $ \reference ->
        case reference of
          Env.Local y ->
            return $ Env.Value [] (Beam.toSource y)

          Env.TopLevel label ->
            do  dest <- Env.freshStackAllocation
                return $ Env.Value
                  [ I.call 0 label
                  , I.move Env.returnRegister dest
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
      do  dest <- Env.freshStackAllocation
          Env.Value leftOps leftResult <- fromExpr left
          Env.Value rightOps rightResult <- fromExpr right
          let ops = leftOps ++ rightOps ++
                [ opBif operator leftResult rightResult dest
                ]
          return $ Env.Value ops (Beam.toSource dest)

    Opt.Binop variable left right -> 
      Env.withReference variable $ \reference ->
        case reference of
          Env.Local _        -> error "operators are only allowed at top-level"
          Env.TopLevel label -> fromFunctionCall label [ left, right ]

    Opt.Function _ _ ->
      error "TODO: function"

    Opt.Call (Opt.Var variable) args ->
      Env.withReference variable $ \reference ->
        case reference of
          Env.Local _        -> error "TODO: local functions"
          Env.TopLevel label -> fromFunctionCall label args

    Opt.Call _ _ ->
      error "TODO: call"

    Opt.TailCall _ _ _ ->
      error "TODO: tail call"

    Opt.If branches elseExpr ->
      do  elseValue <- fromExpr elseExpr
          finalJump <- Env.freshLabel
          dest      <- Env.freshStackAllocation
          let elseOps = Env.ops elseValue ++
                [ I.move (Env.result elseValue) dest
                , I.label finalJump
                ]
          Env.Value
            <$> foldr (fromBranch finalJump dest) (return elseOps) branches
            <*> return (Beam.toSource dest)

    Opt.Let locals body ->
      do  preOps <- mapM fromLetDef locals
          Env.Value ops result <- fromExpr body
          return $ Env.Value (concat preOps ++ ops) result

    Opt.Case _ decider _ ->
      fromDecider decider

    Opt.Data constructor args ->
      fromData constructor =<< mapM fromExpr args


    Opt.DataAccess data_ index ->
      do  dest <- Env.freshStackAllocation
          Env.Value ops result <- fromExpr data_
          let get =
                [ I.move result dest
                , I.get_tuple_element dest (index + 1) dest
                ]
          return $ Env.Value (ops ++ get) (Beam.toSource dest)

    Opt.Access record field ->
      do  dest <- Env.freshStackAllocation
          loopOnFail <- Env.freshLabel
          Env.Value recordOps recordResult <- fromExpr record
          let getOps =
                [ I.label loopOnFail
                , I.get_map_elements loopOnFail recordResult
                    [ ( Beam.toSource (Text.pack field), Beam.toRegister dest )
                    ]
                ]
          return $ Env.Value (recordOps ++ getOps) (Beam.toSource dest)

    Opt.Update record fields ->
      do  Env.Value recordOps recordResult <- fromExpr record
          Env.Value fieldsOps fieldsResult <- fromRecord recordResult fields
          return $ Env.Value (recordOps ++ fieldsOps) fieldsResult

    Opt.Record fields ->
      fromRecord (Beam.Map []) fields

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


fromData :: String -> [ Env.Value ] -> Env.Gen Env.Value
fromData constructor values =
  do  dest <- Env.freshStackAllocation
      let ops = concatMap Env.ops values ++
            I.test_heap (length values + 2) 0
              : I.put_tuple (length values + 1) dest
              : I.put (Beam.Atom (Text.pack constructor))
              : map (I.put . Env.result) values
      return $ Env.Value ops (Beam.toSource dest)


fromRecord :: Beam.IsSource src => src -> [(String, Opt.Expr)] -> Env.Gen Env.Value
fromRecord record fields =
  do  dest <- Env.freshStackAllocation
      valuePairs <- mapM (\(k, v) -> (,) k <$> fromExpr v) fields
      let ops = concatMap (Env.ops . snd) valuePairs ++
            [ I.put_map_assoc Env.cannotFail record dest $ map toMapPair valuePairs
            ]
      return $ Env.Value ops (Beam.toSource dest)


fromFunctionCall :: Beam.Label -> [ Opt.Expr ] -> Env.Gen Env.Value
fromFunctionCall label args =
  do  dest <- Env.freshStackAllocation
      values <- mapM fromExpr args
      let moveArg value i = I.move (Env.result value) (Beam.X i)
          ops = concatMap Env.ops values ++
            zipWith moveArg values [0..] ++
            [ I.call (length args) label
            , I.move Env.returnRegister dest
            ]
      return $ Env.Value ops (Beam.toSource dest)


fromLetDef :: Opt.Def -> Env.Gen [ Beam.Op ]
fromLetDef def =
  case def of
    Opt.Def _ name expr ->
      do  Env.Value ops result <- fromExpr expr
          dest <- Env.registerLocal name
          return $ ops ++ [ I.move result dest ]
          

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail-recursive let"


fromDecider :: Opt.Decider Opt.Choice -> Env.Gen Env.Value
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


fromBranch :: Beam.Label -> Beam.Y -> ( Opt.Expr, Opt.Expr ) -> Env.Gen [ Beam.Op ] -> Env.Gen [ Beam.Op ]
fromBranch finalJump dest ( condExpr, ifExpr ) elseOps =
  do  condValue <- fromExpr condExpr
      ifValue   <- fromExpr ifExpr
      elseJump  <- Env.freshLabel
      let ops = concat
            [ Env.ops condValue
            , [ I.is_eq elseJump (Env.result condValue) true ]
            , Env.ops ifValue
            , [ I.move (Env.result ifValue) dest
              , I.jump finalJump
              , I.label elseJump
              ]
            ]
      fmap (ops ++) elseOps


toMapPair :: (String, Env.Value) -> ( Beam.Source, Beam.Source )
toMapPair (key, Env.Value _ value) =
  ( Beam.toSource $ Beam.Atom (Text.pack key), value )


opBif :: Var.Canonical -> Beam.Source -> Beam.Source -> Beam.Y -> Beam.Op
opBif (Var.Canonical _ "+")  = I.bif2 Env.cannotFail Erlang'splus_2
opBif (Var.Canonical _ "-")  = I.bif2 Env.cannotFail Erlang'sminus_2
opBif (Var.Canonical _ "*")  = I.bif2 Env.cannotFail Erlang'stimes_2
opBif (Var.Canonical _ "/")  = I.bif2 Env.cannotFail Erlang'div_2
opBif (Var.Canonical _ ">")  = I.bif2 Env.cannotFail Erlang'sgt_2
opBif (Var.Canonical _ ">=") = I.bif2 Env.cannotFail Erlang'sge_2
opBif (Var.Canonical _ "<")  = I.bif2 Env.cannotFail Erlang'slt_2
opBif (Var.Canonical _ "<=") = I.bif2 Env.cannotFail Erlang'sle_2
opBif (Var.Canonical _ "==") = I.bif2 Env.cannotFail Erlang'seqeq_2
opBif (Var.Canonical _ "/=") = I.bif2 Env.cannotFail Erlang'sneqeq_2
opBif (Var.Canonical _ "^")  = I.bif2 Env.cannotFail Math'pow
opBif (Var.Canonical _ "%")  = I.bif2 Env.cannotFail Erlang'rem
opBif (Var.Canonical _ "//") = I.bif2 Env.cannotFail Erlang'rem
opBif (Var.Canonical _ "&&") = I.bif2 Env.cannotFail Erlang'band
opBif (Var.Canonical _ "||") = I.bif2 Env.cannotFail Erlang'bor
opBif (Var.Canonical _ "++") = I.bif2 Env.cannotFail Erlang'ebif_plusplus_2
opBif var = error $ "binary operator (" ++ Var.toString var ++ ") is not handled"


true :: Beam.Source
true =
  Beam.toSource ("true" :: Text)


false :: Beam.Source
false =
  Beam.toSource ("false" :: Text)



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
