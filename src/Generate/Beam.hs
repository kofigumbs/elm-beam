{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam (generate) where

import qualified Codec.Beam as Beam
import qualified Data.ByteString.Lazy as LazyBytes

import qualified AST.Module as Module


generate :: Module.Optimized -> LazyBytes.ByteString
generate (Module.Module moduleName _ info) =
  Beam.encode "pine" [] []
