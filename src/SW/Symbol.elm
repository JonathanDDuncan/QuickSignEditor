module SW.Symbol
    exposing
        ( Symbol
        , symbolinit
        , SymbolSize
        , moveSymbols
        , moveSymbol
        , createSymbolbyBaseFillRotation
        , createSymbolbyKey
        , getsymbolBound
        , sizeSymbol
        , symbolsUnderPosition
        , countselectedsymbols
        , gethandtype
        )

import SW.Pua exposing (Base, Fill, Rotation, Key, createkey)
import SW.Types exposing (Size)
import SW.Rectangle exposing (Rect)
import SW.Identifier exposing (updateId)
import Dict exposing (Dict)
import SW.HandFillsType exposing (HandFills(..))
import SW.HandsType exposing (Hands(..))


type alias Symbol =
    { key : String
    , id : Int
    , selected : Bool
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , size : Float
    , nwcolor : String
    , nbcolor : String
    }


symbolinit : Symbol
symbolinit =
    { key = ""
    , id = 0
    , selected = False
    , x = 0
    , y = 0
    , width = 0
    , height = 0
    , size = 1
    , nwcolor = ""
    , nbcolor = ""
    }


gethandtype : HandFills -> Hands
gethandtype filltype =
    case filltype of
        LeftBack ->
            Left

        LeftThumbEdge ->
            Left

        LeftPalm ->
            Left

        LeftBabyEdge ->
            Left

        RightBack ->
            Right

        RightThumbEdge ->
            Right

        RightPalm ->
            Right

        RightBabyEdge ->
            Right


type alias SymbolSize =
    { k : String
    , h : Int
    , w : Int
    }


moveSymbols : Int -> Int -> List Symbol -> List Symbol
moveSymbols movex movey symbols =
    List.map (moveSymbol movex movey) symbols


moveSymbol : Int -> Int -> Symbol -> Symbol
moveSymbol movex movey symbol =
    { symbol | x = symbol.x + movex, y = symbol.y + movey }


createSymbolbyBaseFillRotation : Base -> Fill -> Rotation -> Dict String Size -> Symbol
createSymbolbyBaseFillRotation base fill rotation symbolsizes =
    let
        key =
            createkey base fill rotation
    in
        createSymbolbyKey key symbolsizes


createSymbolbyKey : Key -> Dict String Size -> Symbol
createSymbolbyKey key symbolsizes =
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


getsymbolBound : Symbol -> Rect
getsymbolBound symbol =
    { x = symbol.x
    , y = symbol.y
    , width = round <| toFloat symbol.width * symbol.size
    , height = round <| toFloat symbol.height * symbol.size
    }


sizeSymbol :
    Dict String { width : Int, height : Int }
    -> { c | key : String, height : Int, width : Int }
    -> { c | key : String, height : Int, width : Int }
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


countselectedsymbols : List Symbol -> Int
countselectedsymbols symbols =
    List.length
        (List.filter
            (\symbol ->
                symbol.selected
            )
            symbols
        )


symbolsUnderPosition : { a | x : Int, y : Int } -> List Symbol -> List Symbol
symbolsUnderPosition signviewposition symbols =
    let
        seachrectangle =
            { x = signviewposition.x, y = signviewposition.y, width = 1, height = 1 }
    in
        List.filter (\symbol -> SW.Rectangle.intersect seachrectangle (getsymbolBound symbol)) symbols
