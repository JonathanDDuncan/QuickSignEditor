module MainChooser.GeneralSymbolChooserView exposing (generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display   exposing (signView)

generalsymbolchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
generalsymbolchooser model =
    let
        group =
            1

        -- model.selectedgroup
        validfills =
            [1..6]

        validrotations =
            [1..16]
    in
        table
            [ Html.Attributes.style
                [ "width" => "50%"
                , "height" => px 100
                , "margin" => "5px"
                ]
            ]
            [ tr [] (generalsymbolrow group validfills 1)
            , tr [] (generalsymbolrow group validfills 2)
            , tr [] (generalsymbolrow group validfills 3)
            , tr [] (generalsymbolrow group validfills 4)
            , tr [] (generalsymbolrow group validfills 5)
            , tr [] (generalsymbolrow group validfills 6)
            , tr [] (generalsymbolrow group validfills 7)
            , tr [] (generalsymbolrow group validfills 8)
            , tr [] (generalsymbolrow group validfills 9)
            , tr [] (generalsymbolrow group validfills 10)
            , tr [] (generalsymbolrow group validfills 11)
            , tr [] (generalsymbolrow group validfills 12)
            , tr [] (generalsymbolrow group validfills 13)
            , tr [] (generalsymbolrow group validfills 14)
            , tr [] (generalsymbolrow group validfills 15)
            , tr [] (generalsymbolrow group validfills 16)
            ]


generalsymbolrow : Int -> a -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolrow group validfills rotation =
    [ td [] [ generalsymbolcol group rotation 1 ]
    , td [] [ generalsymbolcol group rotation 2 ]
    , td [] [ generalsymbolcol group rotation 3 ]
    , td [] [ generalsymbolcol group rotation 4 ]
    , td [] [ generalsymbolcol group rotation 5 ]
    , td [] [ generalsymbolcol group rotation 6 ]
    ]


generalsymbolcol : Int -> Int -> Int -> Html MainChooser.Types.Msg
generalsymbolcol group rotation fill =
    let
        symbol =
            getSymbolEditor group fill rotation

        sign =
            { syms = [ symbol ]
            }
    in
        -- App.map SymbolView (symbolView "" symbol)
        App.map SignView
            (signView sign
                [ Html.Attributes.style
                    [ "position" => "relative"
                    , "left" => px 0
                    , "top" => px 0
                    , "width" => px 20
                    , "height" => px 20
                    , "margin" => "5px"
                    ]
                ]
            )
