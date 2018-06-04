module Main exposing (..)

import Basics exposing (..)
import Platform


type alias Session =
    { counter : Int
    }


type Optional a
    = None
    | Some a


main : Platform.Program () () Session
main =
    Platform.server
        { init = Session 1
        , handleCall = handleCall
        }


handleCall : Session -> Session
handleCall session =
    { counter = session.counter + 1 }
