module MainChooser.GeneralSymbolChooserView exposing (generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import SW.Types exposing (..)

generalsymbolchooser : Base -> List Fill -> a -> Int -> Html Msg
generalsymbolchooser base validfills validrotations selectedcolumn =
    table
        [ Html.Attributes.style
            [ "width" => "50%"
            , "height" => px 100
            , "margin" => "5px"
            ]
        ]
        [ tr [] (generalsymbolrow base validfills 1)
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


generalsymbolonecolumn : Base -> Int -> Int -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn base symbolcol rotation1 rotation2 =
    [ td
        []
        [ generalsymbolcol base symbolcol rotation1 ]
    , td
        []
        []
    , td
        []
        [ generalsymbolcol base symbolcol rotation2 ]
    ]


generalsymbolrow : Base -> List Fill -> Rotation -> List (Html MainChooser.Types.Msg)
generalsymbolrow base validfills rotation =
    List.map (\fill -> td [ onClick (SelectedColumn fill) ] [ (generalsymbolcol (Debug.log "base" base)  (Debug.log "fill" fill) rotation) ]) validfills


generalsymbolcol : Base -> Fill -> Rotation -> Html MainChooser.Types.Msg
generalsymbolcol base fill rotation =
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
