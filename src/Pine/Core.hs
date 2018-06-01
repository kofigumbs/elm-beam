module Pine.Core (interfaces, modules) where

import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Variable
import qualified Elm.Package as Package
import qualified Pine.Compiler as Compiler


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
                Type.Lambda a $
                  Type.App
                    (Type.Type (Variable.inCore ["Platform"] "Program"))
                    [ Type.Var "flags", Type.Var "model", Type.Var "msg" ]
          , Module.iUnions  =
              Map.singleton "Program" ([ "flags", "model", "msg" ], [])
          }
      )
    , ( ModuleName.inCore ["Basics"]
      , defaultCoreInterface
          { Module.iExports = Variable.Value . fst <$> basicOps
          , Module.iTypes   = Map.fromList basicOps
          , Module.iUnions  =
              Map.fromList $ (\x -> (x, ([], []))) <$> ["Int", "Float", "Bool"]
          }
      )
    ]

  where
    basicOps =
      [ ( "+",  Type.Lambda number     $ Type.Lambda number     number     )
      , ( "-",  Type.Lambda number     $ Type.Lambda number     number     )
      , ( "*",  Type.Lambda number     $ Type.Lambda number     number     )
      , ( "/",  Type.Lambda float      $ Type.Lambda float      float      )
      , ( ">",  Type.Lambda comparable $ Type.Lambda comparable bool       )
      , ( ">=", Type.Lambda comparable $ Type.Lambda comparable bool       )
      , ( "<",  Type.Lambda comparable $ Type.Lambda comparable bool       )
      , ( "<=", Type.Lambda comparable $ Type.Lambda comparable bool       ) 
      , ( "==", Type.Lambda a          $ Type.Lambda a          bool       ) 
      , ( "/=", Type.Lambda a          $ Type.Lambda a          bool       ) 
      , ( "^",  Type.Lambda number     $ Type.Lambda number     number     )
      , ( "%",  Type.Lambda int        $ Type.Lambda int        int        )
      , ( "//", Type.Lambda int        $ Type.Lambda int        int        )
      , ( "&&", Type.Lambda bool       $ Type.Lambda bool       bool       )
      , ( "||", Type.Lambda bool       $ Type.Lambda bool       bool       )
      , ( "++", Type.Lambda appendable $ Type.Lambda appendable appendable ) 
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
  Type.Type (Variable.inCore ["Basics"] "Float")


int :: Type.Canonical
int =
  Type.Type (Variable.inCore ["Basics"] "Int")


bool :: Type.Canonical
bool =
  Type.Type (Variable.inCore ["Basics"] "Bool")
