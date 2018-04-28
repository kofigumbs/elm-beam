import Platform exposing ((+))

main : Platform.Program () () ()
main =
    Platform.server
        { init = 1
        , handleCall = \state -> state + 1
        }
