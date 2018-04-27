{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as Literal
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  let
    defs =
      Module.program info

    env =
      Env 1 Map.empty
  in
  Beam.encode "pine" [ Beam.export "main" 0, Beam.insertModuleInfo ] $
    State.evalState (concat <$> mapM (fromDef moduleName) defs) env


type Gen a = State.State Env a

data Env = Env
  { _nextLabel :: Int
  , _variables :: Map.Map Var.Canonical Beam.Label
  }

data Value = Value [Beam.Op] Beam.Source

fromDef :: ModuleName.Canonical -> Opt.Def -> Gen [Beam.Op]
fromDef moduleName def =
  case def of
    Opt.Def _facts _name (Opt.Function _args _body) ->
      undefined

    Opt.Def _facts name expr ->
      do  pre <- freshLabel
          post <- freshLabel
          saveTopLevel moduleName name post
          Value body result <- fromExpr expr
          return $ concat
            [ [ I.label pre
              , I.func_info (Text.pack name) 0
              , I.label post
              , I.allocate 0 0
              ]
            , body
            , [ I.move result returnRegister, I.deallocate 0, I.return' ]
            ]

    Opt.TailDef _facts _name _labels _expr ->
      undefined


fromExpr :: Opt.Expr -> Gen Value
fromExpr expr =
  case expr of
    Opt.Literal (Literal.IntNum number) ->
      return $ Value [] (Beam.toSource number)

    Opt.Var variable ->
      do  reference <- Map.lookup variable <$> State.gets _variables
          case reference of
            Nothing ->
              error $ "VARIABLE `" ++ Var.name variable ++ "` is unbound"
            Just label ->
              return $ Value [ I.call 0 label ] (Beam.toSource returnRegister)

    Opt.Program _ (Opt.Call (Opt.Var program) [ argument ]) | program == server ->
      {- TODO: This guard probably needs to move up to `fromDef`,
               so that we can generate the gen_server code -}
      fromExpr argument

    _ ->
      undefined


server :: Var.Canonical
server =
  Var.inCore ["Platform"] "server"


saveTopLevel :: ModuleName.Canonical -> String -> Beam.Label -> Gen ()
saveTopLevel moduleName name label =
  State.modify $ \env -> env
    { _variables =
        Map.insert (Var.topLevel moduleName name) label (_variables env)
    }


freshLabel :: Gen Beam.Label
freshLabel =
  do  i <- State.gets _nextLabel
      State.modify $ \env -> env { _nextLabel = i + 1 }
      return $ Beam.Label i


returnRegister :: Beam.X
returnRegister =
  Beam.X 0
