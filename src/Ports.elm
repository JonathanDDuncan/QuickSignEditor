port module Ports exposing (..)

import SW.Types exposing (..)


-- Ports go here like this


port requestSign : String -> Cmd msg


port requestSignMakerSign : String -> Cmd msg


port receiveSign : (Sign -> msg) -> Sub msg


port receiveSignMakerSign : (Sign -> msg) -> Sub msg


port requestElementPosition : String -> Cmd msg


port receiveElementPosition : (NamedPosition -> msg) -> Sub msg


port shareFsw : String -> Cmd msg
