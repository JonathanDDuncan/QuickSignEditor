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
import Choosers.GroupChooserKeyboard exposing (..)
import Choosers.HandSymbolChooser exposing (..)


createsymbolchooserkeyboard : Model -> List (KeyAction Msg)
createsymbolchooserkeyboard model =
    if ishandgroupchooser model.clicked then
        createhandsymbolchooserkeyboard model
    else
        creategeneralsymbolchooserkeyboard model


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
            createkeyactionlist symbolrowdata (List.range 43 52)

        firstcolumn =
            getgeneralsymbolcolumn 1 symbolcolumndata

        secondcolumn =
            getgeneralsymbolcolumn 2 symbolcolumndata

        columnkeyactionlist1 =
            creatcolumnkeyactionlist firstcolumn (List.range 30 40)

        columnkeyactionlist2 =
            creatcolumnkeyactionlist secondcolumn (List.range 16 28)

        fulllist =
            List.append (List.append rowkeyactionlist columnkeyactionlist1) columnkeyactionlist2
    in
        fulllist


getgeneralsymbolcolumn :
    number
    -> List { c | generalsymbolonecolumndata : { b | symbol1 : a, symbol2 : a } }
    -> List a
getgeneralsymbolcolumn col symbolcolumndata =
    if col == 1 then
        List.map (\colitem -> colitem.generalsymbolonecolumndata.symbol1) symbolcolumndata
    else
        List.map (\colitem -> colitem.generalsymbolonecolumndata.symbol2) symbolcolumndata


creatcolumnkeyactionlist : List EditorSymbol -> List Int -> List (KeyAction Msg)
creatcolumnkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, symbol ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = AddSymbol symbol
                    , display =
                        { width =
                            symbol.width
                        , height =
                            symbol.height
                        , view =
                            Html.map Choosers.Types.SignView
                                (signView { syms = [ symbol ] }
                                    [ Html.Attributes.style
                                        [ "position" => "relative"
                                        , "margin" => "auto"
                                        , "left" => px 0
                                        , "top" => px 4
                                        , "width" => px symbol.width
                                        , "height" => px symbol.height
                                        ]
                                    ]
                                )
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


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
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


createhandsymbolchooserkeyboard : Model -> List (KeyAction Msg)
createhandsymbolchooserkeyboard model =
    let
        petals =
            List.map (\petal -> petal.symbol) model.handsymbol.flowersymbols

        handdata =
            petals

        lefthandactionlist =
            createhandkeyactionlist (SelectHand Left) [ model.handsymbol.symbollefthand ] [ 16 ]

        righthandactionlist =
            createhandkeyactionlist (SelectHand Right) [ model.handsymbol.symbolrighthand ] [ 17 ]

        wallplaneactionlist =
            [ createplanekeyaction (SelectPlane Wall) wallplaneimg 50 20 18 ]

        floorplaneactionlist =
            [ createplanekeyaction (SelectPlane Floor) floorplaneimg 50 20 19 ]

        fillactionlist =
            createfillkeyactionlist (List.reverse model.handsymbol.handfillitems) (List.range 30 33)

        flowerkeyactionlist =
            createflowerkeyactionlist petals [ 24, 23, 36, 49, 50, 51, 39, 25 ]

        fulllist =
            List.concat [ lefthandactionlist, righthandactionlist, wallplaneactionlist, floorplaneactionlist, fillactionlist, flowerkeyactionlist ]
    in
        fulllist


createfillkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, handfillitem ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = (SelectHandFill handfillitem.filltype)
                    , display =
                        { width =
                            handfillitem.symbol.width
                        , height =
                            handfillitem.symbol.height
                        , view =
                            Html.map Choosers.Types.SignView
                                (signView { syms = [ handfillitem.symbol ] }
                                    [ Html.Attributes.style
                                        [ "position" => "relative"
                                        , "margin" => "auto"
                                        , "left" => px 0
                                        , "top" => px 4
                                        , "width" => px handfillitem.symbol.width
                                        , "height" => px handfillitem.symbol.height
                                        ]
                                    ]
                                )
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


createflowerkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, symbol ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = AddSymbol symbol
                    , display =
                        { width =
                            symbol.width
                        , height =
                            symbol.height
                        , view =
                            Html.map Choosers.Types.SignView
                                (signView { syms = [ symbol ] }
                                    [ Html.Attributes.style
                                        [ "position" => "relative"
                                        , "margin" => "auto"
                                        , "left" => px 0
                                        , "top" => px 4
                                        , "width" => px symbol.width
                                        , "height" => px symbol.height
                                        ]
                                    ]
                                )
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


createhandkeyactionlist message data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, symbol ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = message
                    , display =
                        { width =
                            symbol.width
                        , height =
                            symbol.height
                        , view =
                            Html.map Choosers.Types.SignView
                                (signView { syms = [ symbol ] }
                                    [ Html.Attributes.style
                                        [ "position" => "relative"
                                        , "margin" => "auto"
                                        , "left" => px 0
                                        , "top" => px 4
                                        , "width" => px symbol.width
                                        , "height" => px symbol.height
                                        ]
                                    ]
                                )
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


createplanekeyaction message view width height key =
    { test = { key = key, ctrl = False, shift = False, alt = False }
    , action = message
    , display =
        { width =
            width
        , height =
            height
        , view = view
        }
    }



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
