port module Ports exposing (..)

-- Ports go here like this


port check : String -> Cmd msg


port suggestions : (String -> msg) -> Sub msg


port getpua : String -> Cmd msg


port puaresult : (String -> msg) -> Sub msg
