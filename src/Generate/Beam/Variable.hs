{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam.Variable
  ( Def(..)
  , declare, define, defineLocal
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
      do  let arity = length args
          reserveCurriedLabel name arity
          Env.registerTopLevel moduleName name arity

    Opt.Def _ name _ ->
      Env.registerTopLevel moduleName name 0

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail definition"


reserveCurriedLabel :: String -> Int -> Env.Gen ()
reserveCurriedLabel name arity =
  if alwaysExplicit name arity then
    return ()
  else
    Env.freshLabel >> return ()


data Def = Def
  { _def_args :: [ String ]
  , _def_body :: Opt.Expr
  , _def_ops :: [Beam.Op]
  }


define :: ModuleName.Canonical -> Opt.Def -> Env.Gen Def
define moduleName def =
  case def of
    Opt.Def _ name (Opt.Function args expr) ->
      Def args expr <$> topLevelOps moduleName name

    Opt.Def _ name expr ->
      Def [] expr<$> topLevelOps moduleName name

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail definition"


defineLocal :: Opt.Def -> Env.Gen ( Beam.Y, Opt.Expr )
defineLocal def =
  case def of
    Opt.Def _ name expr ->
      (,) <$> Env.registerLocal name <*> return expr

    Opt.TailDef _ _ _ _ ->
      error "TODO: tail-recursive let"


topLevelOps :: ModuleName.Canonical -> String -> Env.Gen [ Beam.Op ]
topLevelOps moduleName name =
  do  Env.resetStackAllocation
      pre <- Env.freshLabel
      ( post, arity ) <- Env.getTopLevel moduleName name
      let ops =
            [ I.label pre
            , I.func_info function arity
            , I.label post
            ]
      if alwaysExplicit name arity
         then return ops
         else curriedOps function (Ctx post arity) (Ctx (curriedOffset post) 1) ops
  where
    function = namespace moduleName name


alwaysExplicit :: String -> Int -> Bool
alwaysExplicit name arity =
  Help.isOp name || arity <= 1


namespace :: ModuleName.Canonical -> String -> Text.Text
namespace (ModuleName.Canonical package name) var =
  Text.pack $ Package.toString package
    ++ "/" ++ ModuleName.toString name
    ++ "#" ++ var


curriedOffset :: Beam.Label -> Beam.Label
curriedOffset (Beam.Label i) =
  Beam.Label (i - 1)



-- SETUP CURRYING


data Ctx = Ctx
  { _ctx_label :: Beam.Label
  , _ctx_arity :: Int
  }


curriedOps :: Text.Text -> Ctx -> Ctx -> [ Beam.Op ] -> Env.Gen [ Beam.Op ]
curriedOps name original current acc =
  if _ctx_arity original == _ctx_arity current then
    do  pre <- Env.freshLabel
        return $ concat
          [ [ I.label pre
            , I.func_info (lambdaName current name) (_ctx_arity original)
            , I.label (_ctx_label current)
            ]
          , reverseXs original
          , [ I.call_only (_ctx_arity original) (_ctx_label original)
            , I.return'
            ]
          , acc
          ]
    
  else
    do  pre <- Env.freshLabel
        next <- Ctx <$> Env.freshLabel <*> return (_ctx_arity current + 1)
        curriedOps name original next $
          I.label pre
            : I.func_info (lambdaName current name) (_ctx_arity current)
            : I.label (_ctx_label current)
            : I.make_fun2 (Beam.Lambda
                { Beam._lambda_name = lambdaName next name
                , Beam._lambda_arity = 1
                , Beam._lambda_label = _ctx_label next
                , Beam._lambda_free = _ctx_arity current
                })
            : I.return'
            : acc


reverseXs :: Ctx -> [ Beam.Op ]
reverseXs (Ctx _ arity) =
  reverseXsHelp (Beam.X arity) 0 (arity - 1) []


reverseXsHelp :: Beam.X -> Int -> Int -> [ Beam.Op ] -> [ Beam.Op ]
reverseXsHelp tmp start end acc =
  if end - start <= 0 then
    acc
  else
    reverseXsHelp tmp (start + 1) (end - 1) $
      I.move (Beam.X end) tmp
        : I.move (Beam.X start) (Beam.X end)
        : I.move tmp (Beam.X start)
        : acc


lambdaName :: Ctx -> Text.Text -> Text.Text
lambdaName (Ctx _ arity) baseName =
  Text.replicate arity "_" <> baseName



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
    Var.Module moduleName   -> tryExactCall moduleName name args
    Var.TopLevel moduleName -> tryExactCall moduleName name args
    Var.Local               -> referToLocal name


tryExactCall :: ModuleName.Canonical -> String -> [ Env.Value ] -> Env.Gen Env.Value
tryExactCall moduleName name args =
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
referByArity moduleName name label arity =
  case arity of
    0 ->
      I.call 0 label

    1 ->
      I.make_fun2 $ Beam.Lambda
        { Beam._lambda_name = "_REF@" <> namespace moduleName name
        , Beam._lambda_arity = 1
        , Beam._lambda_label = label
        , Beam._lambda_free = 0
        }

    _ ->
      I.make_fun2 $ Beam.Lambda
        { Beam._lambda_name = "__REF@" <> namespace moduleName name
        , Beam._lambda_arity = 1
        , Beam._lambda_label = curriedOffset label
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
