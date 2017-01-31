module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import SWEditor.EditorSymbol exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Display exposing (symbolView)
import Keyboard.Shared exposing (KeyboardMode)


root : Model -> Html Msg
root model =
    div []
        [ signView model
        , rectangleselect model
        ]


rectangleselect : Model -> Html msg
rectangleselect model =
    let
        selectrectangle =
            rectangleStartCurrent model
    in
        case model.editormode of
            RectangleSelect ->
                div
                    [ Html.Attributes.style
                        [ "left" => px selectrectangle.x
                        , "top" => px (selectrectangle.y + model.viewposition.y)
                        , "width" => px selectrectangle.width
                        , "height" => px selectrectangle.height
                        , "position" => "absolute"
                        , "border-style" => "dashed"
                        , "border-width" => "1px"
                        ]
                    ]
                    []

            _ ->
                div [] []


editorattributes : Int -> List (Attribute Msg)
editorattributes height =
    [ Html.Attributes.style
        [ "height" => px (height - 50)
        ]
    , Html.Attributes.id "signView"
    , Html.Attributes.class "disablePanZoom signview"
    ]


signView : Model -> Html Msg
signView model =
    div
        (List.append (editorattributes model.containerheight) [ onMouseEnter (SetKeyboardMode Keyboard.Shared.SignView) ])
        (List.map symbolView model.sign.syms)


symbolView : EditorSymbol -> Html Msg
symbolView symbol =
    let
        nbcolor =
            if symbol.selected then
                "blue"
            else
                symbol.nbcolor
    in
        SWEditor.Display.symbolView nbcolor symbol
