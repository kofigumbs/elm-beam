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
      Env 1 0 Map.empty
  in
  Beam.encode "pine" [ Beam.export "main" 0, Beam.insertModuleInfo ] $
    State.evalState (concat <$> mapM (fromDef moduleName) defs) env


type Gen a =
  State.State Env a

data Env = Env
  { _nextLabel :: Int
  , _nextStackAllocation :: Int
  , _functions :: Map.Map Var.Canonical Beam.Label
  }

data Value = Value
  { _ops :: [Beam.Op]
  , _result :: Beam.Source
  }

fromDef :: ModuleName.Canonical -> Opt.Def -> Gen [Beam.Op]
fromDef moduleName def =
  do  pre <- freshLabel
      post <- freshLabel
      resetStackAllocation
      saveTopLevel moduleName name post
      Value ops result <- fromExpr body
      stackNeeded <- getStackAllocations
      return $ concat
        [ [ I.label pre
          , I.func_info (Text.pack name) (length args)
          , I.label post
          , I.allocate stackNeeded (length args)
          ]
        , ops
        , [ I.move result returnRegister
          , I.deallocate stackNeeded
          , I.return'
          ]
        ]
  where
    ( name, args, body ) =
      case def of
        Opt.Def _ _name (Opt.Function _ _expr) ->
          undefined

        Opt.Def _ name expr ->
          ( name, [], expr )

        Opt.TailDef _facts _name _labels _expr ->
          undefined


fromExpr :: Opt.Expr -> Gen Value
fromExpr expr =
  case expr of
    Opt.Literal (Literal.IntNum number) ->
      return $ Value [] (Beam.toSource number)

    Opt.Var variable ->
      do  reference <- Map.lookup variable <$> State.gets _functions
          tmp <- freshStackAllocation
          case reference of
            Nothing ->
              error $ "VARIABLE `" ++ Var.name variable ++ "` is unbound"
            Just label ->
              return $ Value
                [ I.call 0 label
                , I.move returnRegister tmp
                ]
                (Beam.toSource tmp)

    Opt.Record fields ->
      do  tmp <- freshStackAllocation
          values <- mapM (fromExpr . snd) fields
          let ops = concatMap _ops values ++
                [ I.put_map_assoc cannotFail (Beam.Map []) tmp $
                    zipWith (toMapPair . fst) fields values
                ]
          return $ Value ops (Beam.toSource tmp)

    Opt.Program _ (Opt.Call (Opt.Var program) [ argument ]) | program == server ->
      {- TODO: This guard probably needs to move up to `fromDef`,
               so that we can generate the gen_server code -}
      fromExpr argument

    _ ->
      undefined


toMapPair :: String -> Value -> ( Beam.Source, Beam.Source )
toMapPair key (Value _ value) =
  ( Beam.toSource $ Beam.Atom (Text.pack key), value )


server :: Var.Canonical
server =
  Var.inCore ["Platform"] "server"


saveTopLevel :: ModuleName.Canonical -> String -> Beam.Label -> Gen ()
saveTopLevel moduleName name label =
  State.modify $ \env -> env
    { _functions =
        Map.insert (Var.topLevel moduleName name) label (_functions env)
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


returnRegister :: Beam.X
returnRegister =
  Beam.X 0


cannotFail :: Beam.Label
cannotFail =
  Beam.Label 0
