{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam.Environment
  ( run, metadata, Gen
  , Value(..)
  , freshLabel
  , freshStackAllocation, getStackAllocations, resetStackAllocation
  , registerTopLevel, getTopLevel
  , registerArgument, registerLocal, getLocal
  , returnRegister, cannotFail
  ) where

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Generate.Beam.BuiltIn as BuiltIn


run :: ModuleName.Canonical -> Gen [ Beam.Op ] -> [ Beam.Op ]
run moduleName moduleOps =
  let
    evaluate =
      flip State.evalState $ Env 1 0 Map.empty Map.empty
  in
  evaluate $ concatM
    [ BuiltIn.server
        <$> freshLabel
        <*> registerExternal (ModuleName.inCore ["Platform"]) "server" 1
    , moduleOps
    , do  ( main, _ ) <- getTopLevel moduleName "main"
          concatM
            [ BuiltIn.init main <$> freshLabel <*> freshLabel
            , BuiltIn.handleCall main <$> freshLabel <*> freshLabel
            ]
    ]


metadata :: [ Beam.Metadata ]
metadata =
  [ Beam.insertModuleInfo
  , Beam.export "init" 1
  , Beam.export "handle_call" 3
  -- TODO?
  -- , Beam.export "handle_cast" 2
  -- , Beam.export "handle_info" 2
  -- , Beam.export "terminate" 2
  -- , Beam.export "code_change" 3
  ]


type Gen a =
  State.State Env a


data Env = Env
  { _nextLabel :: Int
  , _nextStackAllocation :: Int
  , _topLevels :: Map.Map (ModuleName.Canonical, String) (Beam.Label, Int)
  , _locals :: Map.Map String Beam.Y
  }


data Value = Value
  { ops :: [Beam.Op]
  , result :: Beam.Source
  }



-- MODIFY


registerExternal :: ModuleName.Canonical -> String -> Int -> Gen Beam.Label
registerExternal moduleName name arity =
  do  label <- freshLabel
      State.modify $ \env -> env
        { _topLevels = Map.insert (moduleName, name) (label, arity) (_topLevels env)
        }
      return label


registerTopLevel :: ModuleName.Canonical -> String -> Int -> Gen ()
registerTopLevel moduleName name arity =
  do  registerExternal moduleName name arity
      return ()


getTopLevel :: ModuleName.Canonical -> String -> Gen ( Beam.Label, Int )
getTopLevel moduleName name =
  do  maybeVar <- Map.lookup (moduleName, name) <$> State.gets _topLevels
      case maybeVar of
        Just info -> return info
        Nothing   -> error $ "TOP-LEVEL VARIABLE `" ++ name ++ "` is unbound"


registerArgument :: String -> Gen Beam.Op
registerArgument name =
  do  y@(Beam.Y i) <- registerLocal name
      return $ I.move (Beam.X i) y


registerLocal :: String -> Gen Beam.Y
registerLocal name =
  do  y <- freshStackAllocation
      State.modify $ \env -> env { _locals = Map.insert name y (_locals env) }
      return y


getLocal :: String -> Gen Beam.Y
getLocal name =
  do  maybeVar <- Map.lookup name <$> State.gets _locals
      case maybeVar of
        Just info -> return info
        Nothing   -> error $ "LOCAL VARIABLE `" ++ name ++ "` is unbound"


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
  State.modify $ \env -> env
    { _nextStackAllocation = 0
    , _locals = Map.empty
    }


getStackAllocations :: Gen Int
getStackAllocations =
  State.gets _nextStackAllocation



-- BEAM HELPERS


returnRegister :: Beam.X
returnRegister =
  Beam.X 0


cannotFail :: Beam.Label
cannotFail =
  Beam.Label 0


concatM :: Monad m => [ m [ a ] ] -> m [ a ]
concatM [] = return []
concatM (m:ms) = (++) <$> m <*> concatM ms
