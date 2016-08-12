port module Ports exposing (..)


port check : String -> Cmd msg


port suggestions : (List String -> msg) -> Sub msg
