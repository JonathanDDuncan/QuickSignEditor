module Choosing.View exposing (root, normal, keyview)

import Html exposing (..)
import Html.Attributes exposing (..)
import Choosing.Types exposing (..)
import SWEditor.Display exposing (..)
import SWEditor.DisplaySvg exposing (..)
import SWEditor.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)


--import SubChoosing.View exposing (root)


root : Choosing.Types.Model -> Html Choosing.Types.Msg
root model =
    Html.map Choosing.Types.Display (SWEditor.DisplaySvg.signdisplaysvg model.displaySign model.offset.offsetx model.offset.offsety)


normal : Choosing.Types.Model -> Html Choosing.Types.Msg
normal model =
    div
        []
        [ Html.map Choosing.Types.Display (SWEditor.Display.noScaleSignView model.displaySign)
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
    Html.map Choosing.Types.Display (SWEditor.Display.signView model.displaySign <| keyviewattributes paddingtop)


keyviewattributes : Int -> List (Attribute SWEditor.Types.Msg)
keyviewattributes paddingtop =
    [ Html.Attributes.style
        [ "position" => "relative"
        , "top" => px paddingtop
        ]
    ]
