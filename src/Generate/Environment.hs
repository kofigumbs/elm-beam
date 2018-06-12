{-# LANGUAGE OverloadedStrings #-}
module Generate.Environment
  ( run, metadata, Gen
  , Value(..), Reference(..)
  , freshLabel
  , freshStackAllocation, getStackAllocations, resetStackAllocation
  , registerTopLevel, registerArgument, registerLocal
  , returnRegister, cannotFail
  , withReference
  ) where

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var


run :: ModuleName.Canonical -> Gen [ Beam.Op ] -> [ Beam.Op ]
run moduleName moduleOps =
  let
    evaluate =
      flip State.evalState $ Env 1 0 Map.empty

    main =
      Var.Canonical (Var.TopLevel moduleName) "main"
  in
  evaluate $ (++)
    <$> moduleOps
    <*> withReference main (\(TopLevel mainLabel _) -> prelude mainLabel)


prelude :: Beam.Label -> Gen [ Beam.Op ]
prelude mainLabel =
  do  initPre  <- freshLabel
      initPost <- freshLabel
      handleCallPre  <- freshLabel
      handleCallPost <- freshLabel
      return
        [ I.label initPre
        , I.func_info "init" 1
        , I.label initPost
        , I.allocate 0 1
        , I.call 0 mainLabel
        , I.get_map_elements initPost {- TODO crash? -} returnRegister
            [ ( Beam.toSource (Text.pack "init"), Beam.toRegister returnRegister )
            ]
        , I.deallocate 0
        , I.return'

        , I.label handleCallPre
        , I.func_info "handle_call" 3
        , I.label handleCallPost
        , I.allocate 1 3
        , I.move (Beam.X 2) (Beam.Y 0)
        , I.call 0 mainLabel
        , I.get_map_elements initPost {- TODO crash? -} returnRegister
            [ ( Beam.toSource (Text.pack "handleCall"), Beam.toRegister (Beam.X 1) )
            ]
        , I.move (Beam.Y 0) (Beam.X 0)
        , I.call_fun 1
        , I.deallocate 0
        , I.return'
        ]


metadata :: [ Beam.Metadata ]
metadata =
  [ Beam.insertModuleInfo
  , Beam.export "init" 1
  , Beam.export "handle_call" 3
  -- TODO
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
  , _references :: Map.Map Var.Canonical Reference
  }


data Value = Value
  { ops :: [Beam.Op]
  , result :: Beam.Source
  }


data Reference
  = Local Beam.Y
  | TopLevel Beam.Label Int



-- MODIFY


registerTopLevel :: ModuleName.Canonical -> String -> Int -> Gen Beam.Label
registerTopLevel moduleName name arity =
  do  label <- freshLabel
      save (Var.topLevel moduleName name) (TopLevel label arity)
      return label


registerArgument :: String -> Gen Beam.Op
registerArgument name =
  do  y@(Beam.Y i) <- registerLocal name
      return $ I.move (Beam.X i) y


registerLocal :: String -> Gen Beam.Y
registerLocal name =
  do  y <- freshStackAllocation
      save (Var.local name) (Local y)
      return y


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



-- BEAM HELPERS


returnRegister :: Beam.X
returnRegister =
  Beam.X 0


cannotFail :: Beam.Label
cannotFail =
  Beam.Label 0
