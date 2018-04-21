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
  Map.singleton (ModuleName.inCore ["Platform"]) $
    Module.Interface
      { Module.iVersion  = Compiler.version
      , Module.iPackage  = Package.core
      , Module.iExports  = imports
      , Module.iImports  = mempty
      , Module.iTypes    = types
      , Module.iUnions   = unions
      , Module.iAliases  = mempty
      , Module.iFixities = mempty
      }


imports :: [Variable.Value]
imports =
  [ Variable.Union "Program" Variable.closedListing
  , Variable.Value "server"
  ]


types :: Module.Types
types =
  Map.singleton "server" $
    Type.Lambda (Type.Var "a") $
       Type.App
         (Type.Type (Variable.inCore ["Platform"] "Program"))
         [Type.Var "flags", Type.Var "model", Type.Var "msg"]


unions :: Module.Unions
unions =
  Map.singleton "Program" (["flags", "model", "msg" ], [])
