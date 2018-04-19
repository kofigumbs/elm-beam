{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LazyText
import qualified Data.Map as Map
import qualified System.Environment as Env

import qualified AST.Module as Module
import qualified Elm.Package as Package
import qualified Pine.Compiler as Pine


main :: IO ()
main =
  do path <- head <$> Env.getArgs
     source <- readFile path

     let (dealiaser, _, result) =
           Pine.compile (Pine.Context Package.core False []) source mempty

     case result of
       Right (Pine.Result _ _ output) ->
         LazyText.writeFile "pine.beam" output
       Left errors ->
         error $ concatMap (Pine.errorToString dealiaser path source) errors
