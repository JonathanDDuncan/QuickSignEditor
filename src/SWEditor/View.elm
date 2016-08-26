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
        realPosition =
            getPosition model

        dragoffset =
            SWEditor.Types.getOffset model
    in 
        div [ onMouseDownRectangle ]
            [ div []
                [ input [ onInput Change, value "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468" ] []
                , button [ onClick RequestSign ] [ text "Editor" ]
                , signView model.sign parentwidth parentheight dragoffset
                ]
            , div 
                [ Html.Attributes.style
                    [ "left" => (toString ( Basics.min model.rectanglestart.x model.rectangleend.x)   ++ "px")
                    , "top" => (toString ( Basics.min model.rectanglestart.y model.rectangleend.y ) ++ "px")
                    , "width" => ((toString ((Basics.max model.rectanglestart.x model.rectangleend.x) - (Basics.min model.rectanglestart.x model.rectangleend.x))) ++ "px")
                    , "height" => ((toString ((Basics.max model.rectanglestart.y model.rectangleend.y) - (Basics.min model.rectanglestart.y model.rectangleend.y))) ++ "px")
                    , "position" => "absolute"
                    , "border-style" => "dashed"
                    , "border-width" => "2px"
                    , "background-color" => "darkgoldenrod"
                    ]
                ]
                []
            ]


signView : EditorSign -> Int -> Int -> Offset -> Html Msg
signView sign parentwidth parentheight dragoffset =
    div
        [ Html.Attributes.style
            [ "background-color" => "teal"
            , "width" => "100%"
            , "height" => "500px"
            ]
        ]
        (List.map (symbolViewParentSize parentwidth parentheight dragoffset) sign.syms)


symbolViewParentSize : Int -> Int -> Offset -> EditorSymbol -> Html Msg
symbolViewParentSize parentwidth parentheight dragoffset sign =
    symbolView sign parentwidth parentheight dragoffset


symbolView : EditorSymbol -> Int -> Int -> Offset -> Html Msg
symbolView symbol parentwidth parentheight dragoffset =
    let
        signboxmidWidth =
            parentwidth // 2

        signboxmidHeight =
            parentheight // 2

        { offsetx, offsety } =
            dragoffset
    in
        div
            [ Html.Attributes.class ""
            , onMouseDownDrag
            , onMouseUp (Select symbol.id)
            , Html.Attributes.style
                [ "left" => (centeredvalue symbol.x symbol.selected offsetx signboxmidWidth)
                , "top" => (centeredvalue symbol.y symbol.selected offsety signboxmidHeight)
                , "position" => "absolute"
                ]
            ]
            [ symbolsvg symbol
            ]


centeredvalue : Int -> Bool -> Int -> Int -> String
centeredvalue val selected dragoffset mid =
    toString
        (val
            - 500
            + (if selected then
                dragoffset
               else
                0
              )
            + mid
        )
        ++ "px"


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


onMouseDownDrag : Attribute Msg
onMouseDownDrag =
    on "mousedown" (Json.map DragStart Mouse.position)


onMouseDownRectangle : Attribute Msg
onMouseDownRectangle =
    on "mousedown" (Json.map DrawRectangleStart Mouse.position)


px : Int -> String
px number =
    toString number ++ "px"
