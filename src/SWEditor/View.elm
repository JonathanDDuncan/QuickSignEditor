module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Svg as Svg exposing (svg)
import Svg.Attributes exposing (..)
import SWEditor.Types exposing (..)
import SW.Types exposing (..)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)


--import SubSWEditor.View exposing (root)


(=>) =
    (,)


root : Model -> Int -> Int -> Html Msg
root model parentwidth parentheight =
    let
        realPosition =
            getPosition model
    in
        div []
            [ div []
                [ input [ onInput Change , value "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"] [  ]
                , button [ onClick RequestSign ] [ text "Editor" ]
                , signView model.sign parentwidth parentheight
                , div
                    [ onMouseDown
                    , Html.Attributes.style
                        [ "background-color" => "#3C8D2F"
                        , "cursor" => "move"
                        , "width" => "100px"
                        , "height" => "100px"
                        , "border-radius" => "4px"
                        , "position" => "absolute"
                        , "left" => px realPosition.x
                        , "top" => px realPosition.y
                        , "color" => "white"
                        , "display" => "flex"
                        , "align-items" => "center"
                        , "justify-content" => "center"
                        ]
                    ]
                    [ text "Drag Me!"
                    ]
                ]
            ]


signView : Sign -> Int -> Int -> Html Msg
signView sign parentwidth parentheight =
    div
        [ Html.Attributes.style
            [ "background-color" => "teal"
            , "width" => "100%"
            , "height" => "500px"
            ]
        ]
        (List.map (symbolViewParentSize parentwidth parentheight) sign.syms)


symbolViewParentSize : Int -> Int -> Symbol -> Html Msg
symbolViewParentSize parentwidth parentheight sign =
    symbolView sign parentwidth parentheight


symbolView : Symbol -> Int -> Int -> Html Msg
symbolView symbol parentwidth parentheight =
    let
        signboxmidWidth =
            parentwidth // 2

        signboxmidHeight =
            parentheight // 2
    in
        div
            [ Html.Attributes.class "" 
            ,onMouseDown
            , Html.Attributes.style  
                [ "left" => (centeredvalue symbol.x signboxmidWidth)
                , "top" => (centeredvalue symbol.y signboxmidHeight)
                , "position" => "absolute"
                ]
            ]
            [ symbolsvg symbol
            ]


centeredvalue : Int -> Int -> String
centeredvalue val mid =
    toString (val - 500 + mid) ++ "px"  


symbolsvg : Symbol -> Html Msg
symbolsvg symbol =
    Svg.svg [ Svg.Attributes.height <| toString symbol.height, viewBox <| viewboxStr symbol, Svg.Attributes.width <| toString symbol.width, Svg.Attributes.name "http://www.w3.org/2000/svg" ]
        [ node "text"
            [ Svg.Attributes.style "font-size:0%;" ]
            [ text symbol.key ]
        , Svg.g [ transform ("translate(" ++ toString symbol.x ++ "," ++ toString symbol.y ++ ")") ]
            [ Svg.text'
                [ Svg.Attributes.class "sym-fill"
                , fontSize <| toString symbol.fontsize
                , fill symbol.nbcolor
                ]
                [ text symbol.pua ]
            , Svg.text'
                [ Svg.Attributes.class "sym-line"
                , fontSize <| toString symbol.fontsize
                , fill symbol.nwcolor
                ]
                [ text symbol.pua ]
            ]
        ]


viewboxStr : Symbol -> String
viewboxStr symbol =
    toString symbol.x ++ " " ++ toString symbol.y ++ " " ++ toString symbol.width ++ " " ++ " " ++ toString symbol.height


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.map DragStart Mouse.position)


px : Int -> String
px number =
    toString number ++ "px"
