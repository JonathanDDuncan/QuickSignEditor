module SWEditor.Display exposing (signView, symbolView, scaledSignView, noScaleSignView, symbolaloneView)

import Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)


signView :
    { b
        | syms :
            List
                { a
                    | nbcolor : String
                    , nwcolor : String
                    , pua : String
                    , key : String
                    , size : Float
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


extractColor : { a | nbcolor : String, nwcolor : String, pua : String, key : String, x : Int, y : Int, size : Float } -> Html Msg
extractColor symbol =
    symbolView symbol.nbcolor symbol


symbolView : String -> { a | nwcolor : String, x : Int, y : Int, pua : String, key : String, size : Float } -> Html Msg
symbolView nbcolor symbol =
    span
        [ class "symbol"
        , style
            [ scale symbol.size
            , "left" => px symbol.x
            , "top" => px symbol.y
            ]
        ]
        [ span
            [ style
                [ "display"
                    => "none"
                ]
            ]
            [ text symbol.key ]
        , span
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


symbolaloneView : EditorSymbol -> Int -> Html Msg
symbolaloneView symbol margin =
    div
        [ class "font-30"
        , style
            [ scale symbol.size
            , "width" => px (symbol.width + margin * 2)
            , "height" => px (symbol.height + margin * 2)
            , "margin" => "0 auto"
            ]
        ]
        [ span
            [ style
                [ "display"
                    => "none"
                ]
            ]
            [ text symbol.key ]
        , span
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
                    => symbol.nbcolor
                ]
            ]
            [ text symbol.pua ]
        ]


scaledSignView :
    { b
        | syms :
            List
                { a
                    | nbcolor : String
                    , nwcolor : String
                    , pua : String
                    , key : String
                    , size : Float
                    , x : Int
                    , y : Int
                }
    }
    -> Float
    -> Int
    -> Html SWEditor.Types.Msg
scaledSignView sign size leftmargin =
    signView sign [ style [ scale size, "margin-left" => px leftmargin ] ]


noScaleSignView :
    { b
        | syms :
            List
                { a
                    | nbcolor : String
                    , nwcolor : String
                    , pua : String
                    , key : String
                    , size : Float
                    , x : Int
                    , y : Int
                }
    }
    -> Html SWEditor.Types.Msg
noScaleSignView sign =
    signView sign [ style [ scale 1, "margin-left" => px 0 ] ]


scale : Float -> ( String, String )
scale size =
    "transform" => ("scale(" ++ toString size ++ ")")
