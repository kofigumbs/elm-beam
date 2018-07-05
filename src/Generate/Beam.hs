{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import Codec.Beam.Bifs
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

import Reporting.Bag (Bag)
import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as Literal
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Package
import qualified Generate.Beam.Environment as Env
import qualified Generate.Beam.Variable as BeamVar
import qualified Reporting.Bag as Bag


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  let
    defs = Module.program info
  in
  Beam.encode "pine" Env.metadata $ Env.run moduleName $
    do  mapM_ (BeamVar.declare moduleName) defs
        concatBags id <$> mapM (fromDef moduleName) defs


fromDef :: ModuleName.Canonical -> Opt.Def -> Env.Gen (Bag Beam.Op)
fromDef moduleName def =
  do  BeamVar.Def args body prelude <- BeamVar.define moduleName def
      argOps <- mapM Env.registerArgument args
      Env.Value bodyOps result <- fromExpr body
      stackNeeded <- Env.getStackAllocations
      return $ concatBags id
        [ prelude
        , Bag.singleton (I.allocate stackNeeded (length args))
        , Bag.fromList argOps
        , bodyOps
        , Bag.fromList
            [ I.move result Env.returnRegister
            , I.deallocate stackNeeded
            , I.return'
            ]
        ]


fromExpr :: Opt.Expr -> Env.Gen Env.Value
fromExpr expr =
  case expr of
    Opt.Literal literal ->
      return $ Env.Value Bag.empty (fromLiteral literal)

    Opt.Var variable ->
      BeamVar.standalone variable

    Opt.ExplicitList elements ->
      let
        fromList [] =
          fromData "[]" []
        fromList (first : rest) =
          do  firstValue <- fromExpr first
              restValue  <- fromList rest
              fromData "::" [ firstValue, restValue ]
      in fromList elements

    Opt.Binop operator left right | builtIn operator ->
      do  dest <- Env.freshStackAllocation
          Env.Value leftOps leftResult   <- fromExpr left
          Env.Value rightOps rightResult <- fromExpr right
          let ops = concatBags id
                [ leftOps
                , rightOps
                , Bag.singleton (opBif operator leftResult rightResult dest)
                ]
          return $ Env.Value ops (Beam.toSource dest)

    Opt.Binop variable left right -> 
      BeamVar.explicitCall variable =<< mapM fromExpr [ left, right ]

    Opt.Function _ _ ->
      error "TODO: function"

    Opt.Call (Opt.Var variable) args ->
      BeamVar.explicitCall variable =<< mapM fromExpr args

    Opt.Call function args ->
      do  functionValue <- fromExpr function
          argValues     <- mapM fromExpr args
          BeamVar.genericCall functionValue argValues

    Opt.TailCall function _ args ->
      do  home <- Var.TopLevel <$> Env.getModuleName
          argValues  <- mapM fromExpr args
          BeamVar.explicitCall (Var.Canonical home function) argValues

    Opt.If branches elseExpr ->
      do  elseValue <- fromExpr elseExpr
          finalJump <- Env.freshLabel
          dest      <- Env.freshStackAllocation
          let elseOps = Bag.append
                (Env.ops elseValue)
                (Bag.fromList
                  [ I.move (Env.result elseValue) dest
                  , I.label finalJump
                  ])
          Env.Value
            <$> foldr (fromBranch finalJump dest) (return elseOps) branches
            <*> return (Beam.toSource dest)

    Opt.Let locals body ->
      do  preOps <- concatBags id <$> mapM fromLet locals
          Env.Value ops result <- fromExpr body
          return $ Env.Value (Bag.append preOps ops) result

    Opt.Case _ decider _ ->
      fromDecider decider

    Opt.Data constructor args ->
      fromData constructor =<< mapM fromExpr args

    Opt.DataAccess data_ index ->
      do  dest <- Env.freshStackAllocation
          Env.Value ops result <- fromExpr data_
          let get =
                Bag.fromList
                  [ I.move result dest
                  , I.get_tuple_element dest (index + 1) dest
                  ]
          return $ Env.Value (Bag.append ops get) (Beam.toSource dest)

    Opt.Access record field ->
      do  dest <- Env.freshStackAllocation
          loopOnFail <- Env.freshLabel
          Env.Value recordOps recordResult <- fromExpr record
          let getOps =
                Bag.fromList
                  [ I.label loopOnFail
                  , I.get_map_elements loopOnFail recordResult
                      [ ( Beam.toSource (Text.pack field), Beam.toRegister dest )
                      ]
                  ]
          return $ Env.Value (Bag.append recordOps getOps) (Beam.toSource dest)

    Opt.Update record fields ->
      do  Env.Value recordOps recordResult <- fromExpr record
          Env.Value fieldsOps fieldsResult <- fromRecord recordResult fields
          return $ Env.Value (Bag.append recordOps fieldsOps) fieldsResult

    Opt.Record fields ->
      fromRecord (Beam.Map []) fields

    Opt.Cmd _ ->
      error "TODO"

    Opt.Sub _ ->
      error "TODO"

    Opt.OutgoingPort _ _ ->
      error "TODO"

    Opt.IncomingPort _ _ ->
      error "TODO"

    Opt.Program _ program ->
      fromExpr program

    Opt.GLShader _ _ _ ->
      error "shaders are not supported for server programs"

    Opt.Crash _ _ _ ->
      error "TODO"


fromLiteral :: Literal.Literal -> Beam.Source
fromLiteral literal =
  case literal of
    Literal.Chr char ->
      Beam.toSource (Char.ord char)

    Literal.Str string ->
      Beam.toSource $ Beam.Binary (lazyBytes string)

    Literal.IntNum number ->
      Beam.toSource number

    Literal.FloatNum number ->
      Beam.toSource (Beam.Float number)

    Literal.Boolean bool ->
      if bool then true else false


fromData :: String -> [ Env.Value ] -> Env.Gen Env.Value
fromData constructor values =
  do  dest <- Env.freshStackAllocation
      let ops = concatBags id
            [ concatBags Env.ops values
            , Bag.fromList
                [ I.test_heap (length values + 2) 0
                , I.put_tuple (length values + 1) dest
                , I.put (Beam.Atom (Text.pack constructor))
                ]
            , Bag.fromList (map (I.put . Env.result) values)
            ]
      return $ Env.Value ops (Beam.toSource dest)


fromRecord :: Beam.IsSource src => src -> [(String, Opt.Expr)] -> Env.Gen Env.Value
fromRecord record fields =
  do  dest <- Env.freshStackAllocation
      valuePairs <- mapM (\(k, v) -> (,) k <$> fromExpr v) fields
      let ops = Bag.append
            (concatBags (Env.ops . snd) valuePairs)
            (Bag.singleton
              $ I.put_map_assoc Env.cannotFail record dest
              $ map toMapPair valuePairs)
      return $ Env.Value ops (Beam.toSource dest)


fromLet :: Opt.Def -> Env.Gen (Bag Beam.Op)
fromLet def =
  do  (dest, expr) <- BeamVar.defineLocal def
      Env.Value ops result <- fromExpr expr
      return $ Bag.append ops $ Bag.singleton (I.move result dest)


fromDecider :: Opt.Decider Opt.Choice -> Env.Gen Env.Value
fromDecider decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      fromExpr expr

    Opt.Leaf (Opt.Jump _) ->
      error "TODO: leaf"

    Opt.Chain _ _ _ ->
      error "TODO: chain"

    Opt.FanOut _ _ _ ->
      error "TODO: fan out"


fromBranch :: Beam.Label -> Beam.Y -> ( Opt.Expr, Opt.Expr ) -> Env.Gen (Bag Beam.Op) -> Env.Gen (Bag Beam.Op)
fromBranch finalJump dest ( condExpr, ifExpr ) elseOps =
  do  condValue <- fromExpr condExpr
      ifValue   <- fromExpr ifExpr
      elseJump  <- Env.freshLabel
      let ops = concatBags id
            [ Env.ops condValue
            , Bag.singleton (I.is_eq elseJump (Env.result condValue) true)
            , Env.ops ifValue
            , Bag.fromList
                [ I.move (Env.result ifValue) dest
                , I.jump finalJump
                , I.label elseJump
                ]
            ]
      Bag.append ops <$> elseOps


toMapPair :: (String, Env.Value) -> ( Beam.Source, Beam.Source )
toMapPair (key, Env.Value _ value) =
  ( Beam.toSource $ Beam.Atom (Text.pack key), value )


opBif :: Var.Canonical -> Beam.Source -> Beam.Source -> Beam.Y -> Beam.Op
opBif (Var.Canonical _ "+")  = I.bif2 Env.cannotFail Erlang'splus_2
opBif (Var.Canonical _ "-")  = I.bif2 Env.cannotFail Erlang'sminus_2
opBif (Var.Canonical _ "*")  = I.bif2 Env.cannotFail Erlang'stimes_2
opBif (Var.Canonical _ "/")  = I.bif2 Env.cannotFail Erlang'div_2
opBif (Var.Canonical _ ">")  = I.bif2 Env.cannotFail Erlang'sgt_2
opBif (Var.Canonical _ ">=") = I.bif2 Env.cannotFail Erlang'sge_2
opBif (Var.Canonical _ "<")  = I.bif2 Env.cannotFail Erlang'slt_2
opBif (Var.Canonical _ "<=") = I.bif2 Env.cannotFail Erlang'sle_2
opBif (Var.Canonical _ "==") = I.bif2 Env.cannotFail Erlang'seqeq_2
opBif (Var.Canonical _ "/=") = I.bif2 Env.cannotFail Erlang'sneqeq_2
opBif (Var.Canonical _ "^")  = I.bif2 Env.cannotFail Math'pow
opBif (Var.Canonical _ "%")  = I.bif2 Env.cannotFail Erlang'rem
opBif (Var.Canonical _ "//") = I.bif2 Env.cannotFail Erlang'rem
opBif (Var.Canonical _ "&&") = I.bif2 Env.cannotFail Erlang'band
opBif (Var.Canonical _ "||") = I.bif2 Env.cannotFail Erlang'bor
opBif (Var.Canonical _ "++") = I.bif2 Env.cannotFail Erlang'ebif_plusplus_2
opBif var = error $ "binary operator (" ++ Var.toString var ++ ") is not handled"


true :: Beam.Source
true =
  Beam.toSource ("true" :: Text)


false :: Beam.Source
false =
  Beam.toSource ("false" :: Text)



builtIn :: Var.Canonical -> Bool
builtIn (Var.Canonical home _) =
  case home of
    Var.Module (ModuleName.Canonical pkg _) | Package.core == pkg ->
      True

    _ ->
      False


lazyBytes :: String -> LazyBytes.ByteString
lazyBytes =
  encodeUtf8 . LazyText.fromStrict . Text.pack


concatBags :: (a -> Bag b) -> [ a ] -> Bag b
concatBags _ []     = Bag.empty
concatBags f (a:as) = Bag.append (f a) (concatBags f as)
