module SWEditor.Display exposing (signView, symbolView, symbolView1, scaledSignView, noScaleSignView, symbolaloneView)

import Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.SymbolConverter exposing (..)


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
    let
        sign1 =
            Debug.log "signViewsign" sign
    in
        div
            attr
            (List.map extractColor sign.syms)


extractColor : { a | nbcolor : String, nwcolor : String, pua : String, key : String, x : Int, y : Int, size : Float } -> Html Msg
extractColor symbol =
    symbolView symbol.nbcolor symbol


symbolView : String -> { a | nwcolor : String, x : Int, y : Int, pua : String, key : String, size : Float } -> Html Msg
symbolView nbcolor symbol =
    let
        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <| fillcodefromkey symbol.key
    in
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
                [ text fillchar ]
            , span
                [ class "sym-line"
                , style
                    [ "color"
                        => (nbcolor)
                    ]
                ]
                [ text linechar ]
            ]


symbolView1 nbcolor symbol =
    let
        top =
            (33 - (truncate <| toFloat symbol.height / 2))

        left =
            (25 - (truncate <| toFloat symbol.width / 2))

        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <| fillcodefromkey symbol.key
    in
        span
            [ style
                [ "position"
                    => "relative"
                ]
            , class "centerspancontainer"
            ]
            [ span
                [ class "sym-fill centerspan"
                , style
                    [ "color"
                        => symbol.nwcolor
                    , "top" => px top
                    , "left" => px left
                    ]
                ]
                [ text fillchar ]
            , span
                [ class "sym-line centerspan"
                , style
                    [ "color"
                        => (nbcolor)
                    , "top" => px top
                    , "left" => px left
                    ]
                ]
                [ text linechar ]
            , span
                [ style
                    [ "display"
                        => "none"
                    ]
                ]
                [ text symbol.key ]
            ]


mulInt : Int -> Float -> Int
mulInt num1 num2 =
    truncate (toFloat num1 * num2)


symbolaloneView : EditorSymbol -> Int -> Html Msg
symbolaloneView symbol margin =
    let
        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <| fillcodefromkey symbol.key
    in
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
                [ text fillchar ]
            , span
                [ class "sym-line"
                , style
                    [ "color"
                        => symbol.nbcolor
                    ]
                ]
                [ text linechar ]
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
