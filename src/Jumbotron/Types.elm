module Jumbotron.Types exposing (..)

import Hello.Types


type alias Model =
    { hello : Hello.Types.Model }


type Msg
    = NoOp
    | Hello Hello.Types.Msg
