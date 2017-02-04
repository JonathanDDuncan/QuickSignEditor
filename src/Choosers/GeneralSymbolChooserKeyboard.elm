module Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)

import Html
import Choosers.Types exposing (..)
import Keyboard.Shared exposing (..)
import Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser)
import List.Extra exposing (..)
import SWEditor.Display exposing (signView)
import SWEditor.EditorSymbol exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)


createsymbolchooserkeyboard : Model -> List (KeyAction Msg)
createsymbolchooserkeyboard model =
    creategeneralsymbolchooserkeyboard model


creategeneralsymbolchooserkeyboard : Model -> List (KeyAction Msg)
creategeneralsymbolchooserkeyboard model =
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn

        symbolrowdata =
            generalsymbolchooserdata.generalsymbolrowdata

        symbolcolumndata =
            generalsymbolchooserdata.generalsymbolrowdata

        rowkeyactionlist =
            createkeyactionlist symbolrowdata (List.range 43 52)

        -- columnkeyactionlist =
        --     createkeyactionlist symbolcolumndata <| List.append (List.range 30 40) (List.range 16 28)
        -- actionlist =
        --     List.append rowkeyactionlist columnkeyactionlist
    in
        rowkeyactionlist



--    [ (List.range 43 52), (List.range 30 40), (List.range 16 28), (List.range 1 13) ]
-- { generalsymbolrowdata : List { fill : Int, symbol : EditorSymbol }
-- , symbolcolumnsdata :
--     List
--         { generalsymbolonecolumndata :
--             { show1 : Bool
--             , show2 : Bool
--             , symbol1 : EditorSymbol
--             , symbol2 : EditorSymbol
--             }
--         }
-- }
-- -> List { fill : Int, symbol : EditorSymbol }


createkeyactionlist : List { fill : Int, symbol : EditorSymbol } -> List Int -> List (KeyAction Msg)
createkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, item ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = (SelectedColumn item.fill)
                    , display =
                        { width =
                            item.symbol.width
                        , height =
                            item.symbol.height
                        , view =
                            Html.map Choosers.Types.SignView
                                (signView { syms = [ item.symbol ] }
                                    [ Html.Attributes.style
                                        [ "position" => "relative"
                                        , "margin" => "auto"
                                        , "left" => px 0
                                        , "top" => px 4
                                        , "width" => px item.symbol.width
                                        , "height" => px item.symbol.height
                                        ]
                                    ]
                                )
                            -- Html.map Choosers.Types.SignView (symbolaloneView item.symbol 5)
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist



-- createkeyactionlist1 data range =
--     let
--         keyrange =
--             List.Extra.zip range data
--         groupchooserwithkey =
--             List.concat <|
--                 List.map (\( keyrange, cols ) -> List.Extra.zip keyrange cols) range
--         viewkeylist =
--             List.map
--                 (\( key, item ) ->
--                     { test = { key = key, ctrl = False, shift = False, alt = False }
--                     , action = (SelectedColumn item.fill)
--                     , display =
--                         { width =
--                             item.width
--                         , height =
--                             item.height
--                         , view = Html.map Choosers.Types.SignView (symbolaloneView item 5)
--                         }
--                     }
--                 )
--                 (groupchooserwithkey)
--     in
--         viewkeylist
