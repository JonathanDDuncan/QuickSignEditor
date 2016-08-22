port module Ports exposing (..)

import SW.Types exposing (..)


-- Ports go here like this


port requestSign : String -> Cmd msg


port receiveSign : (Sign -> msg) -> Sub msg
