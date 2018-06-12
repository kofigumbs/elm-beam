{-# LANGUAGE OverloadedStrings #-}
module Generate.Beam.BuiltIn (init, handleCall, server) where

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Instructions as I
import qualified Data.Text as Text

import Prelude hiding (init)


init :: Beam.Label -> Beam.Label -> Beam.Label -> [ Beam.Op ]
init main pre post =
  [ I.label pre
  , I.func_info "init" 1
  , I.label post
  , I.allocate 0 1
  , I.call 0 main
  , I.get_map_elements post {- TODO crash? -} (Beam.X 0)
      [ ( Beam.toSource (Text.pack "init"), Beam.toRegister (Beam.X 0) )
      ]
  , I.deallocate 0
  , I.return'
  ]


handleCall :: Beam.Label -> Beam.Label -> Beam.Label -> [ Beam.Op ]
handleCall main pre post =
  [ I.label pre
  , I.func_info "handle_call" 3
  , I.label post
  , I.allocate 1 3
  , I.move (Beam.X 2) (Beam.Y 0)
  , I.call 0 main
  , I.get_map_elements post {- TODO crash? -} (Beam.X 0)
      [ ( Beam.toSource (Text.pack "handleCall"), Beam.toRegister (Beam.X 1) )
      ]
  , I.move (Beam.Y 0) (Beam.X 0)
  , I.call_fun 1
  , I.deallocate 0
  , I.return'
  ]


server :: Beam.Label -> Beam.Label -> [ Beam.Op ]
server pre post =
  [ I.label pre
  , I.func_info "server" 1
  , I.label post
  , I.return'
  ]
