module Types exposing (..)

import Jumbotron.Types


type alias Model =
    { jumbotron : Jumbotron.Types.Model
    }


type Msg
    = Jumbotron Jumbotron.Types.Msg
