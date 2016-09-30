module Choosing.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Choosing.Types exposing (..)
import SWEditor.Display exposing (..)
import SWEditor.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)


--import SubChoosing.View exposing (root)


root : Choosing.Types.Model -> Html Choosing.Types.Msg
root model =
    div
        []
        [ App.map Choosing.Types.Display (Debug.log "sign" (SWEditor.Display.signView model.displaySign (attributes1 model)))
        ]


attributes1 : Choosing.Types.Model -> List (Attribute SWEditor.Types.Msg)
attributes1 model =
    [ Html.Attributes.style
        [ "position" => "relative"
        , "left" => Debug.log "offsetx" (px model.offset.offsetx)
        , "top" => Debug.log "offsety" (px model.offset.offsety)
        ]
    ]
