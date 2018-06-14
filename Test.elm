module Main exposing (..)

import Basics exposing (..)
import Platform


type alias Session =
    { i : Int }


main : Platform.Program () () Session
main =
    Platform.server
        { init = init
        , handleCall = handleCall 1
        }


init : Session
init =
    Session 1


handleCall : Int -> Session -> Session
handleCall inc ({ i } as s) =
    if i >= 7 then
        { i = -7 }
    else if i == -1 then
        { i = countdown 10 }
    else
        { s | i = i +++ inc }



-- BINOP


(+++) : number -> number -> number
(+++) a b =
    a + b



-- MUTUALLY RECURSIVE


countdown x =
    if x == 1 then
        1
    else
        countdownHelp (x - 1)


countdownHelp x =
    countdown x
