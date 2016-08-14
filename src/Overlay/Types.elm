module Overlay.Types exposing (..)

import Layout.Types


type alias Model =
    { layout : Layout.Types.Model
    , show : Bool
    }


type Msg
    = Hide
    | Layout Layout.Types.Msg
