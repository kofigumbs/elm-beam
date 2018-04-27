import qualified Data.ByteString.Lazy as LazyBytes
import qualified System.Environment as Env

import qualified Elm.Package as Package
import qualified Pine.Compiler as Compiler
import qualified Pine.Core as Core


main :: IO ()
main =
  do path <- getFile
     source <- readFile path

     let context =
           Compiler.Context Package.core False Core.modules
         (dealiaser, _, result) =
           Compiler.compile context source Core.interfaces

     case result of
       Right (Compiler.Result _ _ output) ->
         LazyBytes.writeFile "pine.beam" output
       Left errors ->
         error $ concatMap (Compiler.errorToString dealiaser path source) errors


getFile :: IO String
getFile =
  do  args <- Env.getArgs
      case args of
        [file] -> return file
        _      -> error "USAGE: pine FILE"
