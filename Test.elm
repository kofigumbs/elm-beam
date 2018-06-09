module Main exposing (..)

import Basics exposing (..)
import Platform


type alias Session =
    { i : Int }


(+++) : number -> number -> number
(+++) a b =
    a + b


main : Platform.Program () () Session
main =
    Platform.server
        { init = Session 1
        , handleCall = handleCall
        }


handleCall : Session -> Session
handleCall ({ i } as s) =
    if i >= 7 then
        { i = -7 }
    else if i == -1 then
        { i = 1 }
    else
        { s | i = i +++ 1 }
