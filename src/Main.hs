import qualified Data.ByteString.Lazy as LazyBytes
import qualified System.Environment as Env

import qualified Elm.Package as Package
import qualified ElmBeam.Compiler as Compiler
import qualified ElmBeam.Core as Core


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
         LazyBytes.writeFile "elm.beam" output
       Left errors ->
         error $ concatMap (Compiler.errorToString dealiaser path source) errors


getFile :: IO String
getFile =
  do  args <- Env.getArgs
      case args of
        [file] -> return file
        _      -> error "USAGE: elm-beam FILE"
