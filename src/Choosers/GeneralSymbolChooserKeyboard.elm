module Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)

import Html exposing (..)
import Choosers.Types exposing (..)
import Keyboard.Shared exposing (..)
import Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser, reorderedcolumnforpetal2)
import List.Extra exposing (..)
import SWEditor.DisplaySvg exposing (signdisplaysvg)
import Choosers.GroupChooserKeyboard exposing (..)
import Choosers.HandSymbolChooser exposing (..)
import MaybeHelper.MaybeExtra exposing (..)
import SW.Types exposing (Symbol, signinit)


createsymbolchooserkeyboard : Model -> List (KeyAction Msg)
createsymbolchooserkeyboard model =
    if ishandgroupchooser model.clicked then
        createhandsymbolchooserkeyboard model
    else
        creategeneralsymbolchooserkeyboard model


handflowerkeyidrange : List Int
handflowerkeyidrange =
    [ 24, 23, 36, 49, 50, 51, 39, 25 ]


flower1keyidrange : List Int
flower1keyidrange =
    [ 21, 20, 33, 46, 47, 48, 36, 22 ]


flower2keyidrange : List Int
flower2keyidrange =
    [ 25, 24, 37, 50, 51, 52, 40, 26 ]


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
            createkeyactionlist symbolrowdata (List.range 16 19)

        firstcolumn =
            getgeneralsymbolcolumn symbolcolumndata.column1

        secondcolumn =
            getgeneralsymbolcolumn symbolcolumndata.column2

        flowerkeyactionlistAdd1 =
            createflowerkeyactionlist firstcolumn flower1keyidrange { key = 0, ctrl = False, shift = False, alt = False } (Editor << AddSymbol) "green"
                |> setassamesize

        flowerkeyactionlistAdd2 =
            createflowerkeyactionlist (reorderedcolumnforpetal2 secondcolumn) flower2keyidrange { key = 0, ctrl = False, shift = False, alt = False } (Editor << AddSymbol) "green"
                |> setassamesize

        flowerkeyactionlistReplace1 =
            createflowerkeyactionlist firstcolumn flower1keyidrange { key = 0, ctrl = False, shift = True, alt = False } (Editor << ReplaceSymbol) "red"
                |> setassamesize

        flowerkeyactionlistReplace2 =
            createflowerkeyactionlist (reorderedcolumnforpetal2 secondcolumn) flower2keyidrange { key = 0, ctrl = False, shift = True, alt = False } (Editor << ReplaceSymbol) "red"
                |> setassamesize

        fulllist =
            List.concat [ rowkeyactionlist, flowerkeyactionlistAdd1, flowerkeyactionlistAdd2, flowerkeyactionlistReplace1, flowerkeyactionlistReplace2 ]
    in
        fulllist


setassamesize flowerkeyactionlist =
    let
        maxheight =
            Maybe.withDefault 0 <| List.maximum <| List.map (\fka -> fka.display.height) flowerkeyactionlist

        maxwidth =
            Maybe.withDefault 0 <| List.maximum <| List.map (\fka -> fka.display.width) flowerkeyactionlist
    in
        List.map
            (\fka ->
                let
                    previousdisplay =
                        fka.display
                in
                    { fka
                        | display = { previousdisplay | height = maxheight, width = maxwidth }
                    }
            )
            flowerkeyactionlist


getgeneralsymbolcolumn : List (Maybe a) -> List a
getgeneralsymbolcolumn symbolcolumndata =
    removeNothings <| symbolcolumndata


creatcolumnkeyactionlist : List Symbol -> List Int -> List (KeyAction Msg)
creatcolumnkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, symbol ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = (Editor << AddSymbol) symbol.key
                    , display =
                        { width =
                            symbol.width
                        , height =
                            symbol.height
                        , backgroundcolor = Nothing
                        , view =
                            Html.map Choosers.Types.SignView
                                (signdisplaysvg "" { signinit | syms = [ symbol ] })
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


createkeyactionlist : List { fill : Int, symbol : Symbol } -> List Int -> List (KeyAction Msg)
createkeyactionlist data range =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, item ) ->
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = ((Choosers.Types.Editor << SelectedColumn) item.fill)
                    , display =
                        { width =
                            item.symbol.width
                        , height =
                            item.symbol.height
                        , backgroundcolor = Nothing
                        , view =
                            Html.map Choosers.Types.SignView
                                (signdisplaysvg "" { signinit | syms = [ item.symbol ] })
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

        flowerkeyactionlistadd =
            createflowerkeyactionlist petals handflowerkeyidrange { key = 0, ctrl = False, shift = False, alt = False } (Editor << AddSymbol) "green"
                |> setassamesize

        flowerkeyactionlistreplace =
            createflowerkeyactionlist petals handflowerkeyidrange { key = 0, ctrl = False, shift = True, alt = False } (Editor << ReplaceSymbol) "red"
                |> setassamesize

        fulllist =
            List.concat [ lefthandactionlist, righthandactionlist, wallplaneactionlist, floorplaneactionlist, fillactionlist, flowerkeyactionlistadd, flowerkeyactionlistreplace ]
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
                        , backgroundcolor = Nothing
                        , view =
                            Html.map Choosers.Types.SignView
                                (signdisplaysvg "" { signinit | syms = [ handfillitem.symbol ] })
                        }
                    }
                )
                (keyrange)
    in
        viewkeylist


createflowerkeyactionlist data range modifiers action backgroundcolor =
    let
        keyrange =
            List.Extra.zip range data

        viewkeylist =
            List.map
                (\( key, symbol ) ->
                    { test = { modifiers | key = key }
                    , action = action symbol.key
                    , display =
                        { width =
                            symbol.width
                        , height =
                            symbol.height
                        , backgroundcolor = Just backgroundcolor
                        , view =
                            Html.map Choosers.Types.SignView
                                (signdisplaysvg "" { signinit | syms = [ symbol ] })
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
                        , backgroundcolor = Nothing
                        , view =
                            Html.map Choosers.Types.SignView
                                (signdisplaysvg "" { signinit | syms = [ symbol ] })
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
        , backgroundcolor = Nothing
        , view = view
        }
    }
