module SWEditor.EditorSymbol exposing (..)

import SW.Types exposing (..)
import SWEditor.Rectangle exposing (..)
import SW.Pua exposing (..)
import Dict exposing (..)


updateId : Int -> Int -> Symbol -> Symbol
updateId id index symbol =
    { symbol
        | id = id + index + 1
    }


getSymbolbyBaseFillRotation : Base -> Fill -> Rotation -> Dict String Size -> Symbol
getSymbolbyBaseFillRotation base fill rotation symbolsizes =
    let
        key =
            SW.Pua.key base fill rotation
    in
        getSymbolbyKey key symbolsizes


getSymbolbyKey : Key -> Dict String Size -> Symbol
getSymbolbyKey key symbolsizes =
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
            { symbolinit
                | x = 0
                , y = 0
                , width = size.width
                , height = size.height
                , size = 1
                , nwcolor = "white"
                , key = key
                , nbcolor = "black"
            }
    in
        updateId 0 0 symbol


getsymbolRectangle : Symbol -> Rect
getsymbolRectangle symbol =
    { x = symbol.x
    , y = symbol.y
    , width = symbol.width
    , height = symbol.height
    }


symbolId : Maybe Symbol -> Int
symbolId symbol =
    case symbol of
        Nothing ->
            0

        Just symb ->
            symb.id


countselectedsymbols : List Symbol -> Int
countselectedsymbols symbols =
    List.length
        (List.filter
            (\symbol ->
                symbol.selected
            )
            symbols
        )


moveSymbols : Int -> Int -> List Symbol -> List Symbol
moveSymbols movex movey symbols =
    List.map (moveSymbol movex movey) symbols


moveSymbol : Int -> Int -> Symbol -> Symbol
moveSymbol movex movey symbol =
    { symbol | x = symbol.x + movex, y = symbol.y + movey }


symbolsUnderPosition : Position -> List Symbol -> List Symbol
symbolsUnderPosition signviewposition symbols =
    let
        seachrectangle =
            { x = signviewposition.x, y = signviewposition.y, width = 1, height = 1 }
    in
        List.filter (\symbol -> (SWEditor.Rectangle.intersect seachrectangle (getsymbolRectangle symbol))) symbols
