module Main exposing (..)

import Basics exposing (..)
import Platform


type alias Session =
    List String


type Optional a
    = None
    | Some a


main : Platform.Program () () Session
main =
    Platform.server
        { init = []
        , handleCall = handleCall
        }


handleCall : Session -> Session
handleCall session =
    [ "a", "b", "c" ]
