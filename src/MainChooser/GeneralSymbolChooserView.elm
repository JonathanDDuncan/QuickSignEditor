module MainChooser.GeneralSymbolChooserView exposing (generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)


generalsymbolchooser base validfills validrotations selectedcolumn =
    table
        [ Html.Attributes.style
            [ "width" => "50%"
            , "height" => px 100
            , "margin" => "5px"
            ]
        ]
        [ columnselector base validfills (Maybe.withDefault 1 (List.head validrotations))
        , tr
            [ Html.Attributes.style
                [ "height" => px 10
                ]
            ]
            []
        , tr [] (generalsymbolonecolumn base selectedcolumn 1 9)
        , tr [] (generalsymbolonecolumn base selectedcolumn 2 10)
        , tr [] (generalsymbolonecolumn base selectedcolumn 3 11)
        , tr [] (generalsymbolonecolumn base selectedcolumn 4 12)
        , tr [] (generalsymbolonecolumn base selectedcolumn 5 13)
        , tr [] (generalsymbolonecolumn base selectedcolumn 6 14)
        , tr [] (generalsymbolonecolumn base selectedcolumn 7 15)
        , tr [] (generalsymbolonecolumn base selectedcolumn 8 16)
        ]


generalsymbolonecolumn : Int -> Int -> Int -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn base symbolcol rotation1 rotation2 =
    [ td
        []
        [ generalsymbolcol base rotation1 symbolcol ]
    , td
        []
        []
    , td
        []
        [ generalsymbolcol base rotation2 symbolcol ]
    ]


columnselector : Int -> List Int -> Int -> Html Msg
columnselector base validfills firstrow =
    tr [] (generalsymbolrow base validfills firstrow)


-- generalsymbolchooser2 : MainChooser.Types.Model -> Html MainChooser.Types.Msg
-- generalsymbolchooser2 model =
--     let
--         base =
--             256

--         -- model.selectedbase
--         validfills =
--             [1..6]

--         validrotations =
--             [1..16]
--     in
--         table
--             [ Html.Attributes.style
--                 [ "width" => "50%"
--                 , "height" => px 100
--                 , "margin" => "5px"
--                 ]
--             ]
--             [ tr [] (generalsymbolrow base validfills 1)
--             , tr [] (generalsymbolrow base validfills 2)
--             , tr [] (generalsymbolrow base validfills 3)
--             , tr [] (generalsymbolrow base validfills 4)
--             , tr [] (generalsymbolrow base validfills 5)
--             , tr [] (generalsymbolrow base validfills 6)
--             , tr [] (generalsymbolrow base validfills 7)
--             , tr [] (generalsymbolrow base validfills 8)
--             , tr [] (generalsymbolrow base validfills 9)
--             , tr [] (generalsymbolrow base validfills 10)
--             , tr [] (generalsymbolrow base validfills 11)
--             , tr [] (generalsymbolrow base validfills 12)
--             , tr [] (generalsymbolrow base validfills 13)
--             , tr [] (generalsymbolrow base validfills 14)
--             , tr [] (generalsymbolrow base validfills 15)
--             , tr [] (generalsymbolrow base validfills 16)
--             ]


generalsymbolrow : Int -> List Int -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolrow base validfills rotation =
    List.map (\fill -> td [ onClick (SelectedColumn fill) ] [ (generalsymbolcol base fill rotation ) ]) validfills


generalsymbolcol : Int -> Int -> Int -> Html MainChooser.Types.Msg
generalsymbolcol base fill rotation  =
    let
        symbol =
            getSymbolEditor base fill rotation

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
