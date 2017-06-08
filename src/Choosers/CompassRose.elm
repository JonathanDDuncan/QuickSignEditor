module Choosers.CompassRose exposing (compassrosediv)

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style, class)
import Helpers.ViewExtra exposing (px, (=>))
import List.Extra


--View


compassrosediv : Int -> Int -> Int -> Int -> Int -> List (Html b) -> Html b -> Html b
compassrosediv fullwidth fullheight itemwidth itemheight innersize petalcontent rosecenter =
    let
        radius =
            (toFloat fullwidth / 2) - (toFloat itemwidth / 2)

        centerfloating =
            truncate ((toFloat fullwidth / 2) - sqrt ((radius * radius) / 2))

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
    , petaldiv
        itemwidth
        itemheight
        (centered (centerfloating * 2) itemwidth)
        (centered (centerfloating * 2) itemheight)
    , petaldiv
        itemwidth
        itemheight
        (centered fullheight itemheight)
        0
    , petaldiv
        itemwidth
        itemheight
        (centered ((fullheight - centerfloating) * 2) itemwidth)
        (centered (centerfloating * 2) itemwidth)
    , petaldiv
        itemwidth
        itemheight
        (fullheight - itemheight)
        (centered fullwidth itemwidth)
    , petaldiv
        itemwidth
        itemheight
        (centered ((fullheight - centerfloating) * 2) itemwidth)
        (centered ((fullwidth - centerfloating) * 2) itemwidth)
    , petaldiv
        itemwidth
        itemheight
        (centered fullheight itemheight)
        (fullwidth - itemwidth)
    , petaldiv
        itemwidth
        itemheight
        (centered (centerfloating * 2) itemwidth)
        (centered ((fullwidth - centerfloating) * 2) itemwidth)
    ]


petaldiv : Int -> Int -> Int -> Int -> Html b -> Html b
petaldiv width height top left display =
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
