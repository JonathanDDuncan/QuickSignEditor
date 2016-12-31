module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Display exposing (symbolView)


--import SubSWEditor.View exposing (root)


root : Model -> Html Msg
root model =
    let
        selectrectangle =
            rectangleStartCurrent model
    in
        div []
            [ signView model.sign <| editorattributes <| model.containerheight - 50
            , case model.editormode of
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
            ]


editorattributes : Int -> List (Attribute Msg)
editorattributes height =
    [ Html.Attributes.style
        [ "height" => px height
        ]
    , Html.Attributes.id "signView"
    , Html.Attributes.class "disablePanZoom signview"
    ]


signView : EditorSign -> List (Attribute Msg) -> Html Msg
signView sign attr =
    div
        attr
        (List.map symbolView sign.syms)


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
