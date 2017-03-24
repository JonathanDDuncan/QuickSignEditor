module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.DisplaySvg exposing (symbolsvgmargincolor)
import Keyboard.Shared exposing (KeyboardMode)
import SW.Types exposing (Symbol)


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


editorattributes : Int -> Int -> List (Attribute Msg)
editorattributes height signviewmargin =
    [ Html.Attributes.style
        [ "height" => px (height - 50)
        , "margin" => px signviewmargin
        ]
    , Html.Attributes.id "signView"
    , Html.Attributes.class "disablePanZoom signview"
    ]


signView : Model -> Html Msg
signView model =
    div
        (List.append (editorattributes model.containerheight model.signviewmargin) [ onMouseEnter (SetKeyboardMode Keyboard.Shared.SignView) ])
        (List.map symbolView model.sign.syms)


symbolView : Symbol -> Html Msg
symbolView symbol =
    let
        nbcolor =
            if symbol.selected then
                "blue"
            else
                symbol.nbcolor

        divattributes =
            [ style [ "left" => px symbol.x, "top" => px symbol.y ] ]
    in
        div divattributes [ symbolsvgmargincolor 0 (Just nbcolor) 1 "" symbol ]
