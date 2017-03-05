module Choosers.Petalhelper exposing (..)

import Choosers.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.DisplaySvg exposing (symbolsvg)


--View

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Choosers.Types exposing (..)
import List.Extra exposing (..)
import Maybe.Extra exposing (join)


getoutersymbolpetals : List EditorSymbol -> Int -> Int -> List (Html Msg)
getoutersymbolpetals symbols itemwidth itemheight =
    getoutersymbolpetalsMaybe (toListMaybe symbols) itemwidth itemheight


toListMaybe : List a -> List (Maybe a)
toListMaybe symbols =
    List.map (Just) symbols


getoutersymbolpetalsMaybe : List (Maybe EditorSymbol) -> Int -> Int -> List (Html Msg)
getoutersymbolpetalsMaybe symbols itemwidth itemheight =
    let
        outerpetalsymbolpositions =
            getouterpetalsymbolpositions SWEditor.EditorSymbol.symbolinit symbols itemwidth itemheight

        outerpetalvaluesandpositions =
            List.Extra.zip outerpetalsymbolpositions symbols

        roseouterpetaldata =
            List.map symbolpetal outerpetalvaluesandpositions
    in
        roseouterpetaldata


symbolpetal : ( List (Attribute Msg), Maybe EditorSymbol ) -> Html Msg
symbolpetal ( attrib, symb ) =
    case symb of
        Just symbol ->
            div
                [ attribute "position" "relative"
                , attribute "style" "width:100%; height: inherit"
                , onMouseDown (DragSymbol symbol.key)
                , onDoubleClick
                    (ReplaceSymbol symbol.key)
                ]
                [ div attrib
                    [ Html.map SignView
                        (symbolsvg symbol)
                    ]
                ]

        Nothing ->
            div [] []


getouterpetalsymbolpositions : { a | width : Int, height : Int } -> List (Maybe { a | width : Int, height : Int }) -> Int -> Int -> List (List (Attribute msg))
getouterpetalsymbolpositions defaultsymbol items outeritemwidth outeritemheight =
    let
        top =
            "top:0px;"

        bottom =
            "bottom:0px;"

        left =
            "left:0px;"

        right =
            "right:0px;"
    in
        [ [ attribute "style" <| "position:absolute;" ++ top ++ getleftforcenter (getitem items 1) outeritemwidth ]
        , [ attribute "style" <| "position:absolute;" ++ top ++ left ]
        , [ attribute "style" <| "position:absolute;" ++ gettopformiddle (getitem items 3) outeritemheight ++ left ]
        , [ attribute "style" <| "position:absolute;" ++ bottom ++ left ]
        , [ attribute "style" <| "position:absolute;" ++ bottom ++ getleftforcenter (getitem items 5) outeritemwidth ]
        , [ attribute "style" <| "position:absolute;" ++ bottom ++ right ]
        , [ attribute "style" <| "position:absolute;" ++ gettopformiddle (getitem items 7) outeritemheight ++ right ]
        , [ attribute "style" <| "position:absolute;" ++ top ++ right ]
        ]


getitem : List (Maybe { a | width : Int, height : Int }) -> Int -> Maybe { a | width : Int, height : Int }
getitem items n =
    items
        |> List.drop (n - 1)
        |> List.head
        |> Maybe.Extra.join


getleftforcenter : Maybe { a | width : Int, height : Int } -> Int -> String
getleftforcenter size outeritemwidth =
    case size of
        Just itemsize ->
            "left:" ++ (toString <| round <| (toFloat outeritemwidth - toFloat itemsize.width) / 2) ++ "px;"

        Nothing ->
            ""


gettopformiddle : Maybe { a | width : Int, height : Int } -> Int -> String
gettopformiddle size outeritemheight =
    case size of
        Just itemsize ->
            "top:" ++ (toString <| round <| (toFloat outeritemheight - toFloat itemsize.height) / 2) ++ "px;"

        Nothing ->
            ""
