module Choosers.Petalhelper exposing (getoutersymbolpetals, getoutersymbolpetalsMaybe)

import Choosers.Types exposing (Model, Msg(..), Editor(..))
import SWEditor.DisplaySvg exposing (symbolsvg)
import SW.Types exposing (Symbol, symbolinit)


--View

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (..)
import Maybe.Extra


getoutersymbolpetals : List Symbol -> Int -> Int -> List (Html Msg)
getoutersymbolpetals symbols itemwidth itemheight =
    getoutersymbolpetalsMaybe (toListMaybe symbols) itemwidth itemheight


toListMaybe : List a -> List (Maybe a)
toListMaybe symbols =
    List.map Just symbols


getoutersymbolpetalsMaybe : List (Maybe Symbol) -> Int -> Int -> List (Html Msg)
getoutersymbolpetalsMaybe symbols itemwidth itemheight =
    let
        outerpetalsymbolpositions =
            getouterpetalsymbolpositions symbolinit symbols itemwidth itemheight

        outerpetalvaluesandpositions =
            List.Extra.zip outerpetalsymbolpositions symbols

        roseouterpetaldata =
            List.map symbolpetal outerpetalvaluesandpositions
    in
        roseouterpetaldata


symbolpetal : ( List (Attribute Msg), Maybe Symbol ) -> Html Msg
symbolpetal ( attrib, symb ) =
    case symb of
        Just symbol ->
            div
                [ attribute "position" "relative"
                , attribute "style" "width:100%; height: inherit"
                , onMouseDown ((Editor << DragSymbol) symbol.key)
                , onDoubleClick
                    ((Editor << ReplaceSymbol) symbol.key)
                ]
                [ div attrib
                    [ Html.map SignView
                        (symbolsvg "hover" symbol)
                    ]
                ]

        Nothing ->
            div [] []


getouterpetalsymbolpositions :
    { a | width : Int, height : Int }
    -> List (Maybe { a | width : Int, height : Int })
    -> Int
    -> Int
    -> List (List (Attribute msg))
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
