module Choosers.CompassRose exposing (compassrosediv)

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (..)
import Helpers.ViewExtra exposing (..)
import List.Extra exposing (..)


--View


compassrosediv : Int -> Int -> Int -> Int -> Int -> Int -> List (Html b) -> Html b -> Html b
compassrosediv fullwidth fullheight itemwidth itemheight top innersize petalcontent rosecenter =
    let
        radius =
            (toFloat fullwidth / 2) - (toFloat itemwidth / 2)

        centerfloating =
            truncate ((toFloat fullwidth / 2) - (sqrt (((radius) * (radius)) / 2)))

        centertop =
            truncate <|
                toFloat (fullwidth - innersize)
                    / 2
    in
        div
            [ style
                [ "width" => px fullwidth
                , "height" => px fullheight
                ]
            , class "compassrose"
            ]
            (List.append
                [ div
                    [ style
                        [ "position" => "absolute"
                        , "width" => px innersize
                        , "height" => px innersize
                        , "top" => px centertop
                        , "left" => px centertop
                        ]
                    ]
                    [ rosecenter
                    ]
                ]
                (createpetallayout petalcontent itemwidth fullwidth fullheight itemheight centerfloating)
            )


createpetallayout : List (Html b) -> Int -> Int -> Int -> Int -> Int -> List (Html b)
createpetallayout petalcontent itemwidth fullwidth fullheight itemheight centerfloating =
    let
        petallayout =
            getpetallayout itemwidth fullwidth fullheight itemheight centerfloating
    in
        List.map (\( layout, content ) -> layout content) (List.Extra.zip petallayout petalcontent)


getpetallayout : Int -> Int -> Int -> Int -> Int -> List (Html b -> Html b)
getpetallayout itemwidth fullwidth fullheight itemheight centerfloating =
    [ petaldiv
        itemwidth
        itemheight
        0
        (centered fullwidth itemwidth)
        5
    , petaldiv
        itemwidth
        itemheight
        (centered (centerfloating * 2) itemwidth)
        (centered (centerfloating * 2) itemheight)
        5
    , petaldiv
        itemwidth
        itemheight
        (centered fullheight itemheight)
        0
        10
    , petaldiv
        itemwidth
        itemheight
        (centered ((fullheight - centerfloating) * 2) itemwidth)
        (centered (centerfloating * 2) itemwidth)
        5
    , petaldiv
        itemwidth
        itemheight
        (fullheight - itemheight)
        (centered fullwidth itemwidth)
        5
    , petaldiv
        itemwidth
        itemheight
        (centered ((fullheight - centerfloating) * 2) itemwidth)
        (centered ((fullwidth - centerfloating) * 2) itemwidth)
        5
    , petaldiv
        itemwidth
        itemheight
        (centered fullheight itemheight)
        (fullwidth - itemwidth)
        5
    , petaldiv
        itemwidth
        itemheight
        (centered (centerfloating * 2) itemwidth)
        (centered ((fullwidth - centerfloating) * 2) itemwidth)
        10
    ]


petaldiv : Int -> Int -> Int -> Int -> a -> Html b -> Html b
petaldiv width height top left paddingtop display =
    div
        [ style
            [ "position" => "absolute"
            , "width" => px width
            , "height" => px height
            , "top" => px top
            , "left" => px left
            , "pading-top" => px 20
            ]
        ]
        [ display
        ]


centered : Int -> Int -> Int
centered full item =
    toFloat full
        / 2
        - toFloat item
        / 2
        |> truncate


getouterpetalsymbolpositions : { a | width : Int, height : Int } -> List { a | width : Int, height : Int } -> Int -> Int -> List (List (Attribute msg))
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
        [ [ attribute "style" <| "position:absolute;" ++ top ++ getleftforcenter (getitem items 1 defaultsymbol).width outeritemwidth ]
        , [ attribute "style" <| "position:absolute;" ++ top ++ left ]
        , [ attribute "style" <| "position:absolute;" ++ gettopformiddle (getitem items 3 defaultsymbol).height outeritemheight ++ left ]
        , [ attribute "style" <| "position:absolute;" ++ bottom ++ left ]
        , [ attribute "style" <| "position:absolute;" ++ bottom ++ getleftforcenter (getitem items 5 defaultsymbol).width outeritemwidth ]
        , [ attribute "style" <| "position:absolute;" ++ bottom ++ right ]
        , [ attribute "style" <| "position:absolute;" ++ gettopformiddle (getitem items 7 defaultsymbol).height outeritemheight ++ right ]
        , [ attribute "style" <| "position:absolute;" ++ top ++ right ]
        ]


getitem : List { a | width : Int, height : Int } -> Int -> { a | width : Int, height : Int } -> { a | width : Int, height : Int }
getitem items n default =
    items
        |> List.drop (n - 1)
        |> List.head
        |> Maybe.withDefault default


getleftforcenter : Int -> Int -> String
getleftforcenter itemwidth outeritemwidth =
    "left:" ++ (toString <| round <| (toFloat outeritemwidth - toFloat itemwidth) / 2) ++ "px;"


gettopformiddle : Int -> Int -> String
gettopformiddle itemheight outeritemheight =
    "top:" ++ (toString <| round <| (toFloat outeritemheight - toFloat itemheight) / 2) ++ "px;"
