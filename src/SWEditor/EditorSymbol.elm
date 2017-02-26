module SWEditor.EditorSymbol exposing (..)

import SW.Types exposing (..)
import SWEditor.Rectangle exposing (..)
import SW.SymbolConverter exposing (..)
import Dict exposing (..)


type alias EditorSymbol =
    Idable (Selectable Symbol)


symbolinit : EditorSymbol
symbolinit =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , size = 1
    , nwcolor = ""
    , key = ""
    , nbcolor = ""
    , selected = False
    , id = 0
    }


toEditorSymbol : Int -> Int -> Symbol -> EditorSymbol
toEditorSymbol id index symbol =
    { x = symbol.x
    , y = symbol.y
    , width = symbol.width
    , height = symbol.height
    , size = symbol.size
    , nwcolor = symbol.nwcolor
    , key = symbol.key
    , nbcolor = symbol.nbcolor
    , selected = False
    , id = id + index + 1
    }


fromEditorSymbol : EditorSymbol -> Symbol
fromEditorSymbol symbol =
    { x = symbol.x
    , y = symbol.y
    , width = symbol.width
    , height = symbol.height
    , size = symbol.size
    , nwcolor = symbol.nwcolor
    , key = symbol.key
    , nbcolor = symbol.nbcolor
    }


getSymbolEditorBaseFillRotation : Base -> Fill -> Rotation -> Dict String Size -> EditorSymbol
getSymbolEditorBaseFillRotation base fill rotation symbolsizes =
    let
        key =
            SW.SymbolConverter.key base fill rotation
    in
        getSymbolEditorKey key symbolsizes


getSymbolEditorCode : Code -> Dict String Size -> EditorSymbol
getSymbolEditorCode code symbolsizes =
    let
        key =
            SW.SymbolConverter.keyfromcode code
    in
        getSymbolEditorKey key symbolsizes


getSymbolEditorKey : Key -> Dict String Size -> EditorSymbol
getSymbolEditorKey key symbolsizes =
    let
        symbolsizeresult =
            Dict.get key symbolsizes

        size =
            case symbolsizeresult of
                Just value ->
                    value

                Nothing ->
                    let
                        notfound =
                            Debug.log "symbols size search not found " key
                    in
                        { width = 58, height = 58 }

        symbol =
            { x = 0
            , y = 0
            , width = size.width
            , height = size.height
            , size = 1
            , nwcolor = "white"
            , key = key
            , nbcolor = "black"
            }
    in
        toEditorSymbol 0 0 symbol


getsymbolRectangle : EditorSymbol -> Rect
getsymbolRectangle symbol =
    { x = symbol.x
    , y = symbol.y
    , width = symbol.width
    , height = symbol.height
    }


symbolId : Maybe EditorSymbol -> Int
symbolId symbol =
    case symbol of
        Nothing ->
            0

        Just symb ->
            symb.id


countselectedsymbols : List EditorSymbol -> Int
countselectedsymbols symbols =
    List.length
        (List.filter
            (\symbol ->
                symbol.selected
            )
            symbols
        )


moveSymbols : Int -> Int -> List EditorSymbol -> List EditorSymbol
moveSymbols movex movey symbols =
    List.map (moveSymbol movex movey) symbols


moveSymbol : Int -> Int -> EditorSymbol -> EditorSymbol
moveSymbol movex movey symbol =
    { symbol | x = symbol.x + movex, y = symbol.y + movey }


symbolsUnderPosition : Position -> List EditorSymbol -> List EditorSymbol
symbolsUnderPosition signviewposition symbols =
    let
        seachrectangle =
            { x = signviewposition.x, y = signviewposition.y, width = 1, height = 1 }
    in
        List.filter (\symbol -> (SWEditor.Rectangle.intersect seachrectangle (getsymbolRectangle symbol))) symbols
