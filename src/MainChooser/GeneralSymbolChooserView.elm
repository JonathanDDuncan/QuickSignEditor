module MainChooser.GeneralSymbolChooserView exposing (generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)


generalsymbolchooser group validfills validrotations selectedcolumn =
    let
        a = 5
        -- group =
        --     1

        -- -- model.selectedgroup
        -- validfills =
        --     [1..6]

        -- validrotations =
        --     [1..16]

        -- selectedcolumn =
        --     5
    in
        table
            [ Html.Attributes.style
                [ "width" => "50%"
                , "height" => px 100
                , "margin" => "5px"
                ]
            ]
            [ columnselector group validfills (Maybe.withDefault 1 (List.head validrotations))
            , tr
                [ Html.Attributes.style
                    [ "height" => px 10
                    ]
                ]
                []
            , tr [] (generalsymbolonecolumn group selectedcolumn 1 9)
            , tr [] (generalsymbolonecolumn group selectedcolumn 2 10)
            , tr [] (generalsymbolonecolumn group selectedcolumn 3 11)
            , tr [] (generalsymbolonecolumn group selectedcolumn 4 12)
            , tr [] (generalsymbolonecolumn group selectedcolumn 5 13)
            , tr [] (generalsymbolonecolumn group selectedcolumn 6 14)
            , tr [] (generalsymbolonecolumn group selectedcolumn 7 15)
            , tr [] (generalsymbolonecolumn group selectedcolumn 8 16)
            ]


generalsymbolonecolumn : Int -> Int -> Int -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn group symbolcol rotation1 rotation2 =
    [ td [] [  generalsymbolcol group rotation1 symbolcol ]
    , td [] []
    , td [] [ generalsymbolcol group rotation2 symbolcol ]
    ]


columnselector : Int -> List Int -> Int -> Html Msg
columnselector group validfills firstrow =
    tr [] ( generalsymbolrow group validfills firstrow)


generalsymbolchooser2 : MainChooser.Types.Model -> Html MainChooser.Types.Msg
generalsymbolchooser2 model =
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


generalsymbolrow : Int -> List Int -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolrow group validfills rotation =
    List.map (\fill -> td [onClick (SelectedColumn fill)] [ (generalsymbolcol group rotation fill) ]) validfills


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
