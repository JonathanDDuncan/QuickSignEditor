module MainChooser.CompassRose exposing (compassrosediv, symbolcentered)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import List.Extra exposing (..)


--View


compassrosediv fullwidth fullheight itemwidth itemheight petalcontent rosecenter top =
    let
        radius =
            (toFloat fullwidth / 2) - (toFloat itemwidth / 2)

        centerfloating =
            truncate ((toFloat fullwidth / 2) - (sqrt (((radius) * (radius)) / 2)))
    in
        div
            [ style
                [ "width" => px fullwidth
                , "height" => px fullheight
                , "margin" => "auto"
                , "position" => "relative"
                , "top" => px top
                ]
            ]
            (List.append
                [ div
                    [ style
                        [ "position" => "absolute"
                        , "width" => px (fullwidth - (2 * itemwidth))
                        , "height" => px (fullheight - (2 * itemheight))
                        , "top" => px itemheight
                        , "left" => px itemwidth
                        ]
                    ]
                    [ rosecenter
                    ]
                ]
                (createpetals petalcontent itemwidth fullwidth fullheight itemheight centerfloating)
            )


createpetals petalcontent itemwidth fullwidth fullheight itemheight centerfloating =
    let
        petallayout =
            getpetallayout itemwidth fullwidth fullheight itemheight centerfloating
    in
        List.map (\( layout, content ) -> layout content) (List.Extra.zip petallayout petalcontent)


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


symbolcentered : Bool -> EditorSymbol -> Int -> Int -> Html Msg
symbolcentered drag symbol width height =
    div
        [ style
            [ "position" => "relative"
            , "width" => px width
            , "height" => px height
            , "margin" => "auto"
            ]
        ]
        [ div
            [ if drag then
                onMouseDown (DragSymbol symbol.code)
              else
                onMouseDown Noop
            , onDoubleClick
                (ReplaceSymbol symbol.code)
            ]
            [ Html.map SignView
                (SWEditor.Display.symbolView1 "" symbol)
            ]
        ]


centered : Int -> Int -> Int
centered full item =
    toFloat full
        / 2
        - toFloat item
        / 2
        |> truncate


mulInt : Int -> Float -> Int
mulInt num1 num2 =
    truncate (toFloat num1 * num2)



--State
