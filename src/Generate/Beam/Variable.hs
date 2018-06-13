{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam.Variable (standalone, explicitCall, genericCall) where

import Data.Monoid ((<>))
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Data.Text as Text

import qualified AST.Variable as Var
import qualified Generate.Beam.Environment as Env


standalone :: Var.Canonical -> Env.Gen Env.Value
standalone variable =
  Env.withReference variable referTo


explicitCall :: Var.Canonical -> [ Env.Value ] -> Env.Gen Env.Value
explicitCall variable args =
  Env.withReference variable $ \reference ->
    case reference of
      Env.TopLevel label arity | arity == length args ->
        call (asFunction label) [] args

      _ ->
        do  Env.Value ops result <- referTo reference
            call (asLambda result) ops args


genericCall :: Env.Value -> [ Env.Value ] -> Env.Gen Env.Value
genericCall (Env.Value ops result) =
  call (asLambda result) ops


call :: (Int -> [ Beam.Op ]) -> [ Beam.Op ] -> [ Env.Value ] -> Env.Gen Env.Value
call callConv functionOps argValues =
  do  dest <- Env.freshStackAllocation
      let moveArg value i = I.move (Env.result value) (Beam.X i)
          ops = concat
            [ concatMap Env.ops argValues
            , functionOps
            , zipWith moveArg argValues [0..]
            , callConv (length argValues)
            , [ I.move Env.returnRegister dest ]
            ]
      return $ Env.Value ops (Beam.toSource dest)


asLambda :: Beam.Source -> Int -> [ Beam.Op ]
asLambda fun arity =
  [ I.move fun (Beam.X arity), I.call_fun arity ]


asFunction :: Beam.Label -> Int -> [ Beam.Op ]
asFunction label arity =
  [ I.call arity label ]


referTo :: Env.Reference -> Env.Gen Env.Value
referTo reference =
  case reference of
    Env.Local y ->
      return $ Env.Value [] (Beam.toSource y)

    Env.TopLevel label arity ->
      do  dest <- Env.freshStackAllocation
          return $ Env.Value
            [ if arity == 0
                 then I.call 0 label
                 else I.make_fun2 $ Beam.Lambda (lambdaName label) arity label 0
            , I.move Env.returnRegister dest
            ]
            (Beam.toSource dest)


lambdaName :: Beam.Label -> Text.Text
lambdaName (Beam.Label i) =
  "__LAMBDA@" <> Text.pack (show i)
