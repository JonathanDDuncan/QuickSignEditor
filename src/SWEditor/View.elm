module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Svg as Svg exposing (svg)
import Svg.Attributes exposing (..)
import SWEditor.Types exposing (..)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)


--import SubSWEditor.View exposing (root)


(=>) =
    (,)


root : Model -> Int -> Int -> Html Msg
root model parentwidth parentheight =
    let
        selectrectangle =
            rectangleSelect model
    in
        div []
            [ div []
                [ input [ onInput Change, value "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468" ] []
                , button [ onClick RequestSign ] [ text "Editor" ]
                , signView model.sign parentwidth parentheight
                ]
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


signView : EditorSign -> Int -> Int -> Html Msg
signView sign parentwidth parentheight =
    div
        [ Html.Attributes.style
            [ "background-color" => "teal"
            , "width" => "100%"
            , "height" => "500px"
            , "position" => "relative"
            ]
        , Html.Attributes.id "signView"
        , Html.Attributes.class "disablePanZoom"
          -- , onMouseDownRectangle
        ]
        (List.map (symbolView parentwidth parentheight) sign.syms)


symbolView : Int -> Int -> EditorSymbol -> Html Msg
symbolView parentwidth parentheight symbol =
    let
        id =
            symbol.id
    in
        div
            [ Html.Attributes.class ""
              -- , onMouseDownnoBubble symbol.id
            , Html.Attributes.style
                [ "left" => px symbol.x
                , "top" => px symbol.y
                , "position" => "absolute"
                ]
            ]
            [ symbolsvg symbol
            ]


symbolsvg : EditorSymbol -> Html Msg
symbolsvg symbol =
    Svg.svg
        [ Svg.Attributes.height <| toString symbol.height
        , viewBox <| viewboxStr symbol
        , Svg.Attributes.width <| toString symbol.width
        , Svg.Attributes.name "http://www.w3.org/2000/svg"
        ]
        [ node "text"
            [ Svg.Attributes.style "font-size:0%;" ]
            [ text symbol.key ]
        , Svg.g [ transform ("translate(" ++ toString symbol.x ++ "," ++ toString symbol.y ++ ")") ]
            [ Svg.text'
                [ Svg.Attributes.class "sym-fill"
                , fontSize <| toString symbol.fontsize
                , Svg.Attributes.style <| "fill:" ++ symbol.nwcolor
                ]
                [ text symbol.pua ]
            , Svg.text'
                [ Svg.Attributes.class "sym-line"
                , fontSize <| toString symbol.fontsize
                , Svg.Attributes.style <|
                    "fill:"
                        ++ (if symbol.selected then
                                "blue"
                            else
                                symbol.nbcolor
                           )
                ]
                [ text symbol.pua ]
            ]
        ]


viewboxStr : EditorSymbol -> String
viewboxStr symbol =
    toString symbol.x ++ " " ++ toString symbol.y ++ " " ++ toString symbol.width ++ " " ++ " " ++ toString symbol.height



-- onMouseDownDrag : Attribute Msg
-- onMouseDownDrag =
--     on "mousedown" (Json.map DragStart Mouse.position)


noBubble : Options
noBubble =
    { stopPropagation = True
    , preventDefault = True
    }


onMouseDownnoBubble : Int -> Attribute Msg
onMouseDownnoBubble id =
    onWithOptions "mousedown" noBubble (Json.succeed (SymbolMouseDown id))


px : Int -> String
px number =
    toString number ++ "px"
