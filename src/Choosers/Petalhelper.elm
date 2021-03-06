module Choosers.Petalhelper exposing (getoutersymbolpetals, getoutersymbolpetalsMaybe)

import Choosers.Types exposing (Msg(EditorMsg))
import Choosers.EditorType as Editor
import SW.Display exposing (symbolsvg)
import SW.Symbol exposing (Symbol)


--View

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onMouseDown, onDoubleClick)
import List.Extra
import Maybe.Extra


getoutersymbolpetals : List Symbol -> Int -> Int -> List (Html Msg)
getoutersymbolpetals symbols itemwidth itemheight =
    getoutersymbolpetalsMaybe (toListMaybe symbols) itemwidth itemheight


toListMaybe : List a -> List (Maybe a)
toListMaybe symbols =
    List.map Just symbols


getoutersymbolpetalsMaybe : List (Maybe Symbol) -> Int -> Int -> List (Html Msg)
getoutersymbolpetalsMaybe symbols itemwidth itemheight =
    List.Extra.zip (getouterpetalsymbolpositions symbols itemwidth itemheight) symbols
        |> List.map symbolpetal


symbolpetal : ( List (Attribute Msg), Maybe Symbol ) -> Html Msg
symbolpetal ( attrib, symb ) =
    case symb of
        Just symbol ->
            div
                [ attribute "position" "relative"
                , attribute "style" "width:100%; height: inherit"
                , onMouseDown ((EditorMsg << Editor.DragSymbol) symbol.key)
                , onDoubleClick
                    ((EditorMsg << Editor.ReplaceSymbol) symbol.key)
                ]
                [ div attrib
                    [ symbolsvg "hover" symbol
                    ]
                ]

        Nothing ->
            div [] []


getouterpetalsymbolpositions :
    List (Maybe { a | width : Int, height : Int })
    -> Int
    -> Int
    -> List (List (Attribute msg))
getouterpetalsymbolpositions items outeritemwidth outeritemheight =
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
