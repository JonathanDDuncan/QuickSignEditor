module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import Helpers.ViewExtra exposing (..)
import SWEditor.DisplaySvg exposing (symbolsvgmargincolor)
import SW.Types exposing (Symbol)
import SWEditor.Icons exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ commandsview model
        , signView model
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
                        [ "left" => px (selectrectangle.x + model.viewposition.y - model.signviewmargin)
                        , "top" => px (selectrectangle.y + model.viewposition.y - model.signviewmargin)
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
        , "width" => px (height - 60)
        , "margin" => px signviewmargin
        , "float" => "left"
        ]
    , Html.Attributes.id "signView"
    , Html.Attributes.class "disablePanZoom signview"
    ]


signView : Model -> Html Msg
signView model =
    div
        (List.append (editorattributes model.containerheight model.signviewmargin) [])
        (List.map symbolView model.sign.syms)


commandsview : Model -> Html Msg
commandsview model =
    div
        [ style
            [ "float" => "right"
            , "border-style" => "inset"
            , "border-width" => "4px"
            , "margin-top" => "5px"
            ]
        ]
        [ div [] [ a [ onClick Undo, title "Undo" ] [ button [] [ undoicon ] ] ]
        , div [] [ a [ onClick Redo, title "Redo" ] [ button [] [ redoicon ] ] ]
        , div [] [ a [ onClick DuplicateSymbols, title "Duplicate" ] [ button [] [ duplicateicon ] ] ]
        , div [] [ a [ onClick DeleteSymbols, title "Delete" ] [ button [] [ garbagecanicon ] ] ]
        , div [] [ a [ onClick SizeIncreaseSymbols, title "Increase Size" ] [ button [] [ circleplus ] ] ]
        , div [] [ a [ onClick SizeDecreaseSymbols, title "Decrease Size" ] [ button [] [ circleminus ] ] ]
        ]


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
