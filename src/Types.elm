module Types exposing (Model, Msg(..))

import Overlay.State


type alias Model =
    { overlay : Overlay.State.Model
    }


type Msg
    = Overlay Overlay.State.Msg
