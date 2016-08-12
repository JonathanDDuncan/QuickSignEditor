module Types exposing (..)

import Jumbotron.Types
import Clock.Types
import JSInterop.Types


type alias Model =
    { jumbotron : Jumbotron.Types.Model
    , clock : Clock.Types.Model
    , jsInterop : JSInterop.Types.Model
    }

 
type Msg
    = Jumbotron Jumbotron.Types.Msg
    | Clock Clock.Types.Msg
    | JSInterop JSInterop.Types.Msg
