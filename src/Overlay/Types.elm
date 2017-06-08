module Overlay.Types exposing (Model, Msg(..))

import Layout.Types


type alias Model =
    { layout : Layout.Types.Model
    , show : Bool
    }


type Msg
    = Hide
    | Show
    | Layout Layout.Types.Msg
