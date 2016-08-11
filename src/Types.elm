module Types exposing (..)

import Jumbotron.Types
import Clock.Types


type alias Model =
    { jumbotron : Jumbotron.Types.Model
    , clock : Clock.Types.Model
    }

 
type Msg
    = Jumbotron Jumbotron.Types.Msg
    | Clock Clock.Types.Msg
