module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)


--import SubSWEditor.View exposing (root)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


root : Model -> Html Msg
root model =
    let
        selectrectangle =
            rectangleStartCurrent model
    in
        div []
            [ input [ onInput ChangeFSW, value "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468" ] []
            , button [ onClick RequestSign ] [ text "Editor" ]
            , signView model.sign
            , case model.editormode of
                RectangleSelect ->
                    div
                        [ Html.Attributes.style
                            [ "left" => px (selectrectangle.x)
                            , "top" => px (selectrectangle.y + model.viewposition.y)
                            , "width" => px (selectrectangle.width)
                            , "height" => px (selectrectangle.height)
                            , "position" => "absolute"
                            , "border-style" => "dashed"
                            , "border-width" => "1px"
                            ]
                        ]
                        []

                _ ->
                    div [] []
            ]


signView : EditorSign -> Html Msg
signView sign =
    div
        [ Html.Attributes.style
            [ "background-color" => "teal"
            , "width" => "100%"
            , "height" => "500px"
            , "position" => "relative"
            ]
        , Html.Attributes.id "signView"
        , Html.Attributes.class "disablePanZoom"
        ]
        (List.map (symbolView) sign.syms)


symbolView : EditorSymbol -> Html Msg
symbolView symbol =
    span
        [ Html.Attributes.style
            [ "left" => px symbol.x
            , "top" => px symbol.y
            , "position" => "absolute"
            ]
        ]
        [ span
            [ Html.Attributes.class "sym-fill"
            , Html.Attributes.style
                [ "color"
                    => symbol.nwcolor
                ]
            ]
            [ text symbol.pua ]
        , span
            [ Html.Attributes.class "sym-line"
            , Html.Attributes.style
                [ "color"
                    => (if symbol.selected then
                            "blue"
                        else
                            symbol.nbcolor
                       )
                ]
            ]
            [ text symbol.pua ]
        ]


px : Int -> String
px number =
    toString number ++ "px"
