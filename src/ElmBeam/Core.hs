module ElmBeam.Core (interfaces, modules) where

import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Variable
import qualified Elm.Package as Package
import qualified ElmBeam.Compiler as Compiler


modules :: [ModuleName.Canonical]
modules =
  Map.keys interfaces


interfaces :: Module.Interfaces
interfaces =
  Map.fromList
    [ ( ModuleName.inCore ["Platform"]
      , defaultCoreInterface
          { Module.iExports = 
              [ Variable.Union "Program" Variable.closedListing
              , Variable.Value "server"
              ]
          , Module.iTypes   = 
              Map.singleton "server" $
                Type.Lambda
                  (Type.Record
                    [ ( "init", a )
                    , ( "handleCall", Type.Lambda a a )
                    ]
                    Nothing)
                  (Type.App
                    (Type.Type (Variable.inCore ["Platform"] "Program"))
                    [ Type.Var "flags", Type.Var "msg", a ])
          , Module.iUnions  =
              Map.singleton "Program" ([ "flags", "model", "msg" ], [])
          }
      )
    , ( ModuleName.inCore ["Basics"]
      , defaultCoreInterface
          { Module.iExports = Variable.Value . fst <$> basicOps
          , Module.iTypes   = Map.fromList basicOps
          }
      )
    ]

  where
    basicOps =
      [ ( "+",  binop number     number     number     )
      , ( "-",  binop number     number     number     )
      , ( "*",  binop number     number     number     )
      , ( "/",  binop float      float      float      )
      , ( ">",  binop comparable comparable bool       )
      , ( ">=", binop comparable comparable bool       )
      , ( "<",  binop comparable comparable bool       )
      , ( "<=", binop comparable comparable bool       )
      , ( "==", binop a          a          bool       )
      , ( "/=", binop a          a          bool       )
      , ( "^",  binop number     number     number     )
      , ( "%",  binop int        int        int        )
      , ( "//", binop int        int        int        )
      , ( "&&", binop bool       bool       bool       )
      , ( "||", binop bool       bool       bool       )
      , ( "++", binop appendable appendable appendable )
      ]


defaultCoreInterface :: Module.Interface
defaultCoreInterface =
  Module.Interface
    { Module.iVersion  = Compiler.version
    , Module.iPackage  = Package.core
    , Module.iExports  = mempty
    , Module.iImports  = mempty
    , Module.iTypes    = mempty
    , Module.iUnions   = mempty
    , Module.iAliases  = mempty
    , Module.iFixities = mempty
    }


binop :: Type.Canonical -> Type.Canonical -> Type.Canonical -> Type.Canonical
binop left right returns =
  Type.Lambda left (Type.Lambda right returns)


a :: Type.Canonical
a =
  Type.Var "a"


comparable :: Type.Canonical
comparable =
  Type.Var "comparable"


appendable :: Type.Canonical
appendable =
  Type.Var "appendable"


number :: Type.Canonical
number =
  Type.Var "number"


float :: Type.Canonical
float =
  Type.Type (Variable.builtin "Float")


int :: Type.Canonical
int =
  Type.Type (Variable.builtin "Int")


bool :: Type.Canonical
bool =
  Type.Type (Variable.builtin "Bool")
