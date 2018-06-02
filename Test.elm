module Main exposing (..)

import Basics exposing (..)
import Platform


type alias Session =
    Int


main : Platform.Program () () Session
main =
    Platform.server
        { init = 1
        , handleCall = handleCall
        }


handleCall : Session -> Session
handleCall session =
    session + 1
