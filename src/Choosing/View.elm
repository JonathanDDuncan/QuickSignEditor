module Choosing.View exposing (root, normal, keyview)

import Html exposing (..)
import Html.Attributes exposing (..)
import Choosing.Types exposing (..)
import SWEditor.DisplaySvg exposing (signdisplaysvg, symbolsvg)
import SWEditor.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)


root : Choosing.Types.Model -> Html Choosing.Types.Msg
root model =
    Html.map Choosing.Types.Display (SWEditor.DisplaySvg.signdisplaysvg model.displaySign model.offset.offsetx model.offset.offsety)


normal : Choosing.Types.Model -> Html Choosing.Types.Msg
normal model =
    div
        []
        [ Html.map Choosing.Types.Display (signdisplaysvg model.displaySign 0 0)
        ]


attributes1 : Choosing.Types.Model -> List (Attribute msg)
attributes1 model =
    [ Html.Attributes.style
        [ "position" => "relative"
        , "left" => px model.offset.offsetx
        , "top" => px model.offset.offsety
        ]
    ]


keyview : Choosing.Types.Model -> Int -> Html Choosing.Types.Msg
keyview model paddingtop =
    Html.map Choosing.Types.Display (signdisplaysvg model.displaySign 0 0)


keyviewattributes : Int -> List (Attribute SWEditor.Types.Msg)
keyviewattributes paddingtop =
    [ Html.Attributes.style
        [ "position" => "relative"
        , "top" => px paddingtop
        ]
    ]
