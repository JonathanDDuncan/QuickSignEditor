module Choosers.GeneralSymbolChooserKeyboard exposing (..)

import Html
import Choosers.Types exposing (..)
import Keyboard.Shared exposing (..)
import Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser)
import List.Extra exposing (..)
import SWEditor.Display exposing (symbolaloneView)


creategeneralsymbolchooserkeyboard : Model -> List (KeyAction Msg)
creategeneralsymbolchooserkeyboard model =
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn

        symbolrowdata =
            generalsymbolchooserdata.generalsymbolrowdata

        symbolcolumndata =
            generalsymbolchooserdata.symbolcolumnsdata

        rowkeyactionlist =
            createkeyactionlist symbolrowdata []

        columnkeyactionlist =
            createkeyactionlist symbolcolumndata []
    in
        []


createkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        groupchooserwithkey =
            List.concat <|
                List.map (\( keyrange, cols ) -> List.Extra.zip keyrange cols) range

        viewkeylist =
            List.map
                (\( key, item ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = (GroupSelected item.chooseritem)
                    , display =
                        { width =
                            item.symbol.width
                        , height =
                            item.symbol.height
                        , view = Html.map Choosers.Types.SignView (symbolaloneView item.symbol 5)
                        }
                    }
                )
                (groupchooserwithkey)
    in
        viewkeylist
