{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import Codec.Beam.Bifs (Erlang'display(..))
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as Literal
import qualified AST.Module as Module
import qualified AST.Variable as Var


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  let
    defs = Module.program info
  in
  Beam.encode "pine" [ Beam.export "main" 0, Beam.insertModuleInfo ] $
    State.evalState (concat <$> mapM fromDef defs) (Env 1 Map.empty)


type Gen a = State.State Env a

data Env = Env
  { _nextLabel :: Int
  , _variables :: Map.Map Var.Canonical Beam.Source
  }

data Value = Value [Beam.Op] Beam.Source

fromDef :: Opt.Def -> Gen [Beam.Op]
fromDef def =
  case def of
    Opt.Def _facts name expr ->
      do  pre <- freshLabel
          post <- freshLabel
          Value body result <- fromExpr expr
          return $ concat
            [ [ I.label pre , I.func_info (Text.pack name) 0 , I.label post ]
            , body
            , [ I.move result returnRegister , I.return' ]
            ]

    Opt.TailDef _facts _name _labels _expr ->
      undefined


fromExpr :: Opt.Expr -> Gen Value
fromExpr expr =
  case expr of
    Opt.Literal (Literal.IntNum number) ->
      return $ Value [] (Beam.toSource number)

    Opt.Var (Var.Canonical _home name) ->
      undefined

    Opt.Program _ (Opt.Call (Opt.Var var) [argument]) | var == server ->
      do  Value body result <- fromExpr argument
          let boilerplate =
                [ I.bif1 (Beam.Label 0) Erlang'display result (Beam.X 0)
                ]
          return $ Value (body ++ boilerplate) (Beam.toSource Beam.Nil)


    _ ->
      undefined


server :: Var.Canonical
server =
  Var.inCore ["Platform"] "server"


freshLabel :: Gen Beam.Label
freshLabel =
  do  i <- State.gets _nextLabel
      State.modify $ \env -> env { _nextLabel = i + 1 }
      return $ Beam.Label i


returnRegister :: Beam.X
returnRegister =
  Beam.X 0
