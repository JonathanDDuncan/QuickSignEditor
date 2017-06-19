module SWEditor.View exposing (root)

import Html exposing (Html, Attribute, div, button, a, input)
import Html.Attributes exposing (style, title, id, class, type_)
import Html.Events exposing (onClick)
import SWEditor.Types
    exposing
        ( Model
        , Msg
            ( Undo
            , Redo
            , DuplicateSymbols
            , DeleteSymbols
            , SizeIncreaseSymbols
            , SizeDecreaseSymbols
            , NormallyWhiteNewColor
            , NormallyBlackNewColor
            )
        , EditorMode(RectangleSelect)
        )
import SWEditor.RectangleSelect exposing (rectangleStartCurrent)
import Helpers.ViewExtra exposing (px, (=>))
import SW.Display exposing (symbolsvgmargincolor)
import SW.Symbol exposing (Symbol)
import SWEditor.Icons
    exposing
        ( undoicon
        , redoicon
        , duplicateicon
        , garbagecanicon
        , circleplus
        , circleminus
        , nbicon
        , nwicon
        )


root : Model -> Html Msg
root model =
    div []
        [ commandsview
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


commandsview : Html Msg
commandsview =
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
        , div []
            [ a [ onClick NormallyBlackNewColor, title "Normally Black" ]
                [ div [] [ input [ type_ "hidden", class "cp-hidden-input-black" ] [] ]
                , button [] [ nbicon ]
                ]
            ]
        , div []
            [ a [ onClick NormallyWhiteNewColor, title "Normally White" ]
                [ input [ type_ "hidden", class "cp-hidden-input-white" ] []
                , button [ class "cp-basic" ] [ nwicon ]
                ]
            ]
        ]


symbolView : Symbol -> Html Msg
symbolView symbol =
    div [ style [ "left" => px symbol.x, "top" => px symbol.y ] ]
        [ symbolsvgmargincolor 0 (colorselected symbol) 1 "" symbol
        ]


colorselected : { a | nbcolor : String, selected : Bool } -> Maybe String
colorselected symbol =
    if symbol.selected then
        Just "blue"
    else
        Nothing
