module SWEditor.Display exposing (signView, symbolView)

import Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Types exposing (..)


signView :
    { b
        | syms :
            List
                { a
                    | nbcolor : String
                    , nwcolor : String
                    , pua : String
                    , x : Int
                    , y : Int
                }
    }
    -> List (Attribute Msg)
    -> Html Msg
signView sign attr =
    div
        attr
        (List.map extractColor sign.syms)


extractColor : { a | nbcolor : String, nwcolor : String, pua : String, x : Int, y : Int } -> Html Msg
extractColor symbol =
    symbolView symbol.nbcolor symbol


symbolView : String -> { a | nwcolor : String, x : Int, y : Int, pua : String } -> Html Msg
symbolView nbcolor symbol =
    span
        [ class "symbol"
        , style
            [ "left" => px symbol.x
            , "top" => px symbol.y
            ]
        ]
        [ span
            [ class "sym-fill"
            , style
                [ "color"
                    => symbol.nwcolor
                ]
            ]
            [ text symbol.pua ]
        , span
            [ class "sym-line"
            , style
                [ "color"
                    => (nbcolor)
                ]
            ]
            [ text symbol.pua ]
        ]
