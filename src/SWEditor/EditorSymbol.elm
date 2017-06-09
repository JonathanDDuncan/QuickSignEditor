module SWEditor.EditorSymbol
    exposing
        ( symbolId
        , moveSymbols
        , colorsymbol
        , sizesymbol
        , sizeSymbol
        , adjustposition
        , getsymbolRectangle
        , getSymbolbyKey
        , getSymbolbyBaseFillRotation
        , symbolsUnderPosition
        , countselectedsymbols
        )

import SW.Types exposing (Size, Position, Colors)
import SW.Symbol exposing (Symbol, Rotation, Fill, Base, Key, symbolinit)
import SW.Rectangle exposing (Rect)
import SW.Pua exposing (createkey)
import Dict exposing (Dict)
import SW.Identifier exposing (updateId)


getSymbolbyBaseFillRotation : Base -> Fill -> Rotation -> Dict String Size -> Symbol
getSymbolbyBaseFillRotation base fill rotation symbolsizes =
    let
        key =
            SW.Pua.createkey base fill rotation
    in
        getSymbolbyKey key symbolsizes


sizeSymbol : Dict String Size -> Symbol -> Symbol
sizeSymbol symbolsizes symbol =
    let
        symbolsizeresult =
            Dict.get symbol.key symbolsizes

        size =
            case symbolsizeresult of
                Just value ->
                    value

                Nothing ->
                    let
                        _ =
                            Debug.log "symbols size search not found " symbol.key
                    in
                        { width = 58, height = 58 }
    in
        { symbol
            | width = size.width
            , height = size.height
        }


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
                        _ =
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
    , width = round <| toFloat symbol.width * symbol.size
    , height = round <| toFloat symbol.height * symbol.size
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
        List.filter (\symbol -> SW.Rectangle.intersect seachrectangle (getsymbolRectangle symbol)) symbols


colorsymbol : Colors -> Symbol -> Symbol
colorsymbol colors symbol =
    if colors.nbcolor == Nothing && colors.nwcolor == Nothing then
        symbol
    else
        let
            nbcolor =
                Maybe.withDefault symbol.nbcolor colors.nbcolor

            nwcolor =
                Maybe.withDefault symbol.nwcolor colors.nwcolor
        in
            { symbol | nbcolor = nbcolor, nwcolor = nwcolor }


sizesymbol : { size : Float, pos : Int, adjustment : { x : Int, y : Int } } -> Symbol -> Symbol
sizesymbol size symbol =
    { symbol | size = size.size }


adjustposition : { size : Float, pos : Int, adjustment : { x : Int, y : Int } } -> Symbol -> Symbol
adjustposition size symbol =
    { symbol | x = symbol.x + size.adjustment.x, y = symbol.y + size.adjustment.y }
