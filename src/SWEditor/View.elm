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
            [ input [ onInput ChangeFSW, value "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468" ] []
            , button [ onClick RequestSign ] [ text "Editor" ]
            , signView model.sign editorattributes
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


editorattributes : List (Attribute Msg)
editorattributes =
    [ Html.Attributes.style
        [ "background-color" => "teal"
        , "width" => "100%"
        , "height" => "500px"
        , "position" => "relative"
        ]
    , Html.Attributes.id "signView"
    , Html.Attributes.class "disablePanZoom"
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
