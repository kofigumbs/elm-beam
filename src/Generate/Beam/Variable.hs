{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam.Variable
  ( declare, define, defineLocal
  , standalone, explicitCall, genericCall
  ) where

import Data.Monoid ((<>))
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Data.Text as Text

import qualified AST.Expression.Optimized as Opt
import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Package
import qualified Generate.Beam.Environment as Env


-- CREATE


declare :: ModuleName.Canonical -> Opt.Def -> Env.Gen ()
declare moduleName def =
  case def of
    Opt.Def _ name (Opt.Function args _) ->
      do  Env.freshLabel {- reserve label for curried version -}
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
      ( post, arity ) <- Env.getTopLevel moduleName name
      ops <- withCurriedVersion post moduleName name arity
        [ I.label pre
        , I.func_info (namespace moduleName name) arity
        , I.label post
        ]
      return ( args, ops, body )


defineLocal :: Opt.Def -> Env.Gen ( Beam.Y, Opt.Expr )
defineLocal def =
  case def of
    Opt.Def _ name expr ->
      (,) <$> Env.registerLocal name <*> return expr

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail-recursive let"


withCurriedVersion :: Beam.Label -> ModuleName.Canonical -> String -> Int -> [ Beam.Op ] -> Env.Gen [ Beam.Op ]
withCurriedVersion original@(Beam.Label i) moduleName name arity ops =
  if Help.isOp name || arity <= 1 then
    return ops
  else if arity == 2 then
    do  let baseName = namespace moduleName name
        pre <- Env.freshLabel
        let post = Beam.Label (i - 1)
        nextPre <- Env.freshLabel
        nextPost <- Env.freshLabel
        return $
          I.label pre
            : I.func_info ("_" <> baseName) 1
            : I.label post
            : I.make_fun2 (Beam.Lambda
                { Beam._lambda_name = "__" <> baseName
                , Beam._lambda_arity = 1
                , Beam._lambda_label = nextPost
                , Beam._lambda_free = 1
                })
            : I.return'

            : I.label nextPre
            : I.func_info ("__" <> baseName) 2
            : I.label nextPost

            -- reverse arguments
            : I.move (Beam.X 1) (Beam.X 2)
            : I.move (Beam.X 0) (Beam.X 1)
            : I.move (Beam.X 2) (Beam.X 0)

            : I.call_only 2 original
            : I.return'
            : ops
  else
    error "TODO: curried > 2"


namespace :: ModuleName.Canonical -> String -> Text.Text
namespace (ModuleName.Canonical package name) var =
  Text.pack $ Package.toString package
    ++ "/" ++ ModuleName.toString name
    ++ "#" ++ var



-- USE


standalone :: Var.Canonical -> Env.Gen Env.Value
standalone (Var.Canonical home name) =
  case home of
    Var.BuiltIn             -> error "TODO"
    Var.Module moduleName   -> referToTopLevel moduleName name
    Var.TopLevel moduleName -> referToTopLevel moduleName name
    Var.Local               -> referToLocal name


explicitCall :: Var.Canonical -> [ Env.Value ] -> Env.Gen Env.Value
explicitCall (Var.Canonical home name) args =
  case home of
    Var.BuiltIn             -> error "TODO"
    Var.Module moduleName   -> tryExhaustiveCall moduleName name args
    Var.TopLevel moduleName -> tryExhaustiveCall moduleName name args
    Var.Local               -> referToLocal name


tryExhaustiveCall :: ModuleName.Canonical -> String -> [ Env.Value ] -> Env.Gen Env.Value
tryExhaustiveCall moduleName name args =
  do  ( label, arity ) <- Env.getTopLevel moduleName name
      if arity == length args
        then call [] $ qualified label args
        else flip genericCall args =<< referToTopLevel moduleName name


genericCall :: Env.Value -> [ Env.Value ] -> Env.Gen Env.Value
genericCall (Env.Value ops result) args =
  call ops $ curried result args


call :: [ Beam.Op ] -> [ Beam.Op ] -> Env.Gen Env.Value
call functionOps callOps =
  do  dest <- Env.freshStackAllocation
      let ops = concat
            [ functionOps
            , callOps
            , [ I.move Env.returnRegister dest ]
            ]
      return $ Env.Value ops (Beam.toSource dest)


referToTopLevel :: ModuleName.Canonical -> String -> Env.Gen Env.Value
referToTopLevel moduleName name =
  do  ( label@(Beam.Label i), arity ) <- Env.getTopLevel moduleName name
      dest <- Env.freshStackAllocation
      return $ Env.Value
        [ referByArity moduleName name label arity
        , I.move Env.returnRegister dest
        ]
        (Beam.toSource dest)


referByArity :: ModuleName.Canonical -> String -> Beam.Label -> Int -> Beam.Op
referByArity moduleName name label@(Beam.Label i) arity =
  case arity of
    0 ->
      I.call 0 label

    1 ->
      I.make_fun2 $ Beam.Lambda
        { Beam._lambda_name = "__REF@" <> namespace moduleName name
        , Beam._lambda_arity = 1
        , Beam._lambda_label = label
        , Beam._lambda_free = 0
        }

    _ ->
      I.make_fun2 $ Beam.Lambda
        { Beam._lambda_name = "__REF@" <> namespace moduleName name
        , Beam._lambda_arity = 1
        , Beam._lambda_label = Beam.Label (i - 1)
        , Beam._lambda_free = 0
        }


referToLocal :: String -> Env.Gen Env.Value
referToLocal name =
  Env.Value [] . Beam.toSource <$> Env.getLocal name



-- DIFFERENT WAYS TO CALL A FUNCTION


qualified :: Beam.Label -> [ Env.Value ] -> [ Beam.Op ]
qualified label argValues =
  concatMap Env.ops argValues
    ++ zipWith moveArg argValues [0..]
    ++ [ I.call (length argValues) label ]


moveArg :: Env.Value -> Int -> Beam.Op
moveArg value i =
  I.move (Env.result value) (Beam.X i)


curried :: Beam.Source -> [ Env.Value ] -> [ Beam.Op ]
curried fun argValues =
  concatMap Env.ops argValues
    ++ curriedRevHelp fun argValues []


curriedRevHelp :: Beam.IsSource s => s -> [ Env.Value ] -> [ Beam.Op ] -> [ Beam.Op ]
curriedRevHelp fun argValues acc =
  case argValues of
    [] ->
      reverse acc

    Env.Value _ arg : rest ->
      curriedRevHelp Env.returnRegister rest $
        I.call_fun 1
          : I.move arg (Beam.X 0)
          : I.move fun (Beam.X 1)
          : acc
