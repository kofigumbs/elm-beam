{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam.Variable
  ( declare, define --, defineLocal
  , standalone, explicitCall, genericCall
  ) where

import Data.Monoid ((<>))
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Data.Text as Text

import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Package
import qualified Generate.Beam.Environment as Env


-- CREATE


declare :: ModuleName.Canonical -> Opt.Def -> Env.Gen ()
declare moduleName def =
  case def of
    Opt.Def _ name (Opt.Function args _) ->
      Env.registerTopLevel moduleName name (length args)

    Opt.Def _ name _ ->
      Env.registerTopLevel moduleName name 0

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail definition"


define :: ModuleName.Canonical -> Opt.Def -> Env.Gen ([ String ], [ Beam.Op ], Opt.Expr)
define moduleName def =
  case def of
    Opt.Def _ name (Opt.Function args expr) ->
      defineTopLevel moduleName name args expr

    Opt.Def _ name expr ->
      defineTopLevel moduleName name [] expr

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail definition"

defineTopLevel :: ModuleName.Canonical -> String -> [ String ] -> Opt.Expr -> Env.Gen ([ String ], [ Beam.Op ], Opt.Expr)
defineTopLevel moduleName name args body =
  do  Env.resetStackAllocation
      pre <- Env.freshLabel
      ( post, _ ) <- Env.getTopLevel moduleName name
      return
        ( args
        , [ I.label pre
          , I.func_info (namespace moduleName name) (length args)
          , I.label post
          ]
        , body
        )


namespace :: ModuleName.Canonical -> String -> Text.Text
namespace (ModuleName.Canonical package name) var =
  Text.pack $ Package.toString package
    ++ "/" ++ ModuleName.toString name
    ++ "#" ++ var



-- USE


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
