{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import Codec.Beam.Bifs
import Data.Monoid ((<>))
import Data.Text (Text)
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
import qualified Elm.Package as Package


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  let
    defs =
      Module.program info

    env =
      Env 1 0 Map.empty
  in
  Beam.encode "pine"
    [ Beam.insertModuleInfo
    , Beam.export "init" 1
    , Beam.export "handle_call" 3
    , Beam.export "handle_cast" 2
    , Beam.export "handle_info" 2
    , Beam.export "terminate" 2
    , Beam.export "code_change" 3
    ] $ State.evalState (concat <$> mapM (fromDef moduleName) defs) env


type Gen a =
  State.State Env a

data Env = Env
  { _nextLabel :: Int
  , _nextStackAllocation :: Int
  , _references :: Map.Map Var.Canonical (Either Beam.Label Beam.X)
  }

data Value = Value
  { _ops :: [Beam.Op]
  , _result :: Beam.Source
  }


fromDef :: ModuleName.Canonical -> Opt.Def -> Gen [Beam.Op]
fromDef moduleName def =
  case def of
    Opt.Def _ "main" (Opt.Program _ (Opt.Call _ [ Opt.Record fields ])) ->
      let
        Just init = lookup "init" fields
        Just handleCall = lookup "handleCall" fields
      in
      (++)
        <$> fromBody moduleName id "init" [ "_" ] (Opt.Data "ok" [ init ])
        <*> do  let body = Opt.Call handleCall [ Opt.Var (Var.local "state") ]
                fromBody moduleName id "handle_call" [ "_", "_", "state" ]
                  (Opt.Data "reply" [ body, body ])

    Opt.Def _ name (Opt.Function args expr) ->
      fromBody moduleName (namespace moduleName) name args expr

    Opt.Def _ name expr ->
      fromBody moduleName (namespace moduleName) name [] expr


fromBody
  :: ModuleName.Canonical
  -> (String -> String)
  -> String
  -> [String]
  -> Opt.Expr
  -> Gen [Beam.Op]
fromBody moduleName prefix name args body =
  do  pre <- freshLabel
      post <- freshLabel
      resetStackAllocation
      saveTopLevel moduleName name post
      sequence_ $ zipWith saveLocal args $ map Beam.X [0..]
      Value bodyOps result <- fromExpr body
      stackNeeded <- getStackAllocations
      return $ concat
        [ [ I.label pre
          , I.func_info (Text.pack (prefix name)) (length args)
          , I.label post
          , I.allocate stackNeeded (length args)
          ]
        , bodyOps
        , [ I.move result returnRegister
          , I.deallocate stackNeeded
          , I.return'
          ]
        ]


fromExpr :: Opt.Expr -> Gen Value
fromExpr expr =
  case expr of
    Opt.Literal (Literal.IntNum number) ->
      return $ Value [] (Beam.toSource number)

    Opt.Binop operator left right | inCore operator ->
      do  tmp <- freshStackAllocation
          Value leftOps leftResult <- fromExpr left
          Value rightOps rightResult <- fromExpr right
          let ops =
                leftOps ++ rightOps ++
                  [ opBif operator leftResult rightResult tmp
                  ]
          return $ Value ops (Beam.toSource tmp)

    Opt.Var variable ->
      withReference variable $ \reference ->
        case reference of
          Right register ->
            return $ Value [] (Beam.toSource register)

          Left label ->
            do  tmp <- freshStackAllocation
                return $ Value
                  [ I.call 0 label
                  , I.move returnRegister tmp
                  ]
                  (Beam.toSource tmp)

    Opt.Data constructor args ->
      do  tmp <- freshStackAllocation
          values <- mapM fromExpr args
          let ops = concatMap _ops values ++
                I.test_heap (length args + 2) 0
                  : I.put_tuple (length args + 1) tmp
                  : I.put (Beam.Atom (Text.pack constructor))
                  : map (I.put . _result) values
          return $ Value ops (Beam.toSource tmp)

    Opt.Record fields ->
      do  tmp <- freshStackAllocation
          values <- mapM (fromExpr . snd) fields
          let ops = concatMap _ops values ++
                [ I.put_map_assoc cannotFail (Beam.Map []) tmp $
                    zipWith (toMapPair . fst) fields values
                ]
          return $ Value ops (Beam.toSource tmp)

    Opt.Call (Opt.Var variable) args ->
      withReference variable $ \reference ->
        case reference of
          Left label ->
            do  tmp <- freshStackAllocation
                values <- mapM fromExpr args
                let ops = concatMap _ops values ++
                      zipWith (\(Value _ v) -> I.move v . Beam.X) values [0..] ++
                      [ I.call (length args) label
                      , I.move returnRegister tmp
                      ]
                return $ Value ops (Beam.toSource tmp)


toMapPair :: String -> Value -> ( Beam.Source, Beam.Source )
toMapPair key (Value _ value) =
  ( Beam.toSource $ Beam.Atom (Text.pack key), value )


opBif :: Var.Canonical -> Beam.Source -> Beam.Source -> Beam.Y -> Beam.Op
opBif (Var.Canonical _ "+")  = I.bif2 noFailure Erlang'splus_2
opBif (Var.Canonical _ "-")  = I.bif2 noFailure Erlang'sminus_2
opBif (Var.Canonical _ "*")  = I.bif2 noFailure Erlang'stimes_2
opBif (Var.Canonical _ "/")  = I.bif2 noFailure Erlang'div_2
opBif (Var.Canonical _ ">")  = I.bif2 noFailure Erlang'sgt_2
opBif (Var.Canonical _ ">=") = I.bif2 noFailure Erlang'sge_2
opBif (Var.Canonical _ "<")  = I.bif2 noFailure Erlang'slt_2
opBif (Var.Canonical _ "<=") = I.bif2 noFailure Erlang'sle_2
opBif (Var.Canonical _ "==") = I.bif2 noFailure Erlang'seqeq_2
opBif (Var.Canonical _ "/=") = I.bif2 noFailure Erlang'sneqeq_2
opBif (Var.Canonical _ "^")  = I.bif2 noFailure Math'pow
opBif (Var.Canonical _ "%")  = I.bif2 noFailure Erlang'rem
opBif (Var.Canonical _ "//") = I.bif2 noFailure Erlang'rem
opBif (Var.Canonical _ "&&") = I.bif2 noFailure Erlang'band
opBif (Var.Canonical _ "||") = I.bif2 noFailure Erlang'bor
opBif (Var.Canonical _ "++") = I.bif2 noFailure Erlang'ebif_plusplus_2


noFailure :: Beam.Label
noFailure =
  Beam.Label 0


inCore :: Var.Canonical -> Bool
inCore var =
  case var of
    Var.Canonical (Var.Module (ModuleName.Canonical home _)) _ ->
      Package.core == home

    _ ->
      False


namespace :: ModuleName.Canonical -> String -> String
namespace (ModuleName.Canonical package name) var =
  Package.toString package
    ++ "/" ++ ModuleName.toString name
    ++ "/" ++ var



-- ENVIRONMENT


saveTopLevel :: ModuleName.Canonical -> String -> Beam.Label -> Gen ()
saveTopLevel moduleName name label =
  State.modify $ \env -> env
    { _references =
        Map.insert (Var.topLevel moduleName name) (Left label) (_references env)
    }


saveLocal :: String -> Beam.X -> Gen ()
saveLocal name register =
  if name == "_" then
    return ()
  else
    State.modify $ \env -> env
      { _references =
          Map.insert (Var.local name) (Right register) (_references env)
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


withReference :: Var.Canonical -> (Either Beam.Label Beam.X -> Gen a) -> Gen a
withReference variable f =
  do  maybeReference <- Map.lookup variable <$> State.gets _references
      case maybeReference of
        Nothing ->
          error $ "VARIABLE `" ++ Var.name variable ++ "` is unbound"

        Just reference ->
          f reference


returnRegister :: Beam.X
returnRegister =
  Beam.X 0


cannotFail :: Beam.Label
cannotFail =
  Beam.Label 0
