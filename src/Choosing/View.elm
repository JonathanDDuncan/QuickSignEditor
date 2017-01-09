module Choosing.View exposing (root, normal)

import Html exposing (..)
 
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
        [ Html.map Choosing.Types.Display (SWEditor.Display.signView model.displaySign (attributes1 model))
        ]


normal : Choosing.Types.Model -> Html Choosing.Types.Msg
normal model =
    div
        []
        [ Html.map Choosing.Types.Display (SWEditor.Display.noScaleSignView model.displaySign)
        ]


attributes1 : Choosing.Types.Model -> List (Attribute SWEditor.Types.Msg)
attributes1 model =
    [ Html.Attributes.style
        [ "position" => "relative"
        , "left" => px model.offset.offsetx
        , "top" => px model.offset.offsety
        ]
    ]


normal1 : Choosing.Types.Model -> List (Attribute SWEditor.Types.Msg)
normal1 model =
    []
