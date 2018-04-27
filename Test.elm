import Platform

main : Platform.Program () () ()
main =
    Platform.server 123
--    Platform.server
--        { init = init
--        , terminate = terminate
--        , handleCall = handleCall
--        , handleCast = handleCast
--        }

init = 1
terminate = 2
handleCall = 3
handleCast = 2
