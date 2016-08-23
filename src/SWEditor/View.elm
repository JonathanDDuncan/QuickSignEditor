module SWEditor.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Svg as Svg exposing (svg)
import Svg.Attributes exposing (..)
import SWEditor.Types exposing (..)
import SW.Types exposing (..)


--import SubSWEditor.View exposing (root)


root : Model -> Int -> Int -> Html Msg
root model parentwidth parentheight=
    div []
        [ div []
            [ input [ onInput Change ] []
            , button [ onClick RequestSign ] [ text "Editor" ]
            , signView model.sign parentwidth parentheight
            ]
        ]


signView: Sign -> Int -> Int -> Html Msg
signView sign parentwidth parentheight =
    div [ style "background-color: teal;width: 100%;height: 500px;"]
         (List.map (symbolViewParentSize parentwidth parentheight)  sign.syms) 
 
symbolViewParentSize : Int -> Int -> Symbol -> Html Msg
symbolViewParentSize  parentwidth parentheight  sign =  symbolView sign parentwidth parentheight 

symbolView: Symbol -> Int -> Int -> Html Msg
symbolView symbol parentwidth parentheight =
    let 
    signboxmidWidth = parentwidth // 2 
    signboxmidHeight = parentheight // 2

    in 
    div [ class "",  style <| "left: " ++ (centeredvalue symbol.x signboxmidWidth) ++ "top: " ++ (centeredvalue symbol.y signboxmidHeight) ++ "position: absolute;"  ]
        [ symbolsvg symbol
        ]

centeredvalue: Int -> Int-> String
centeredvalue   val mid= toString (val - 500 + mid)  ++ "px" ++ ";"
 
      
symbolsvg: Symbol -> Html Msg
symbolsvg symbol =  
    Svg.svg [ height <| toString symbol.height, viewBox <| viewboxStr symbol, width <| toString symbol.width, name "http://www.w3.org/2000/svg" ]
            [ node "text"
                [ style "font-size:0%;" ]
                [ text symbol.key ]
            , Svg.g [ transform ("translate(" ++ toString symbol.x ++"," ++ toString symbol.y ++")") ]
                [ Svg.text'
                    [ class "sym-fill" , fontSize <| toString symbol.fontsize, fill symbol.nbcolor
                    ]
                    [ text symbol.pua ]
                , Svg.text'
                    [ class "sym-line", fontSize <| toString symbol.fontsize, fill symbol.nwcolor
                    ]
                    [ text symbol.pua ]
                ]
            ]

viewboxStr: Symbol -> String
viewboxStr symbol = 
           toString symbol.x ++ " " ++ toString symbol.y ++ " " ++ toString symbol.width ++ " " ++ " " ++ toString symbol.height
         