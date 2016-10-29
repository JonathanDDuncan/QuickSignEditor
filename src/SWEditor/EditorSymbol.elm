module SWEditor.EditorSymbol exposing (..)

import SW.Types exposing (..)
import SWEditor.Rectangle exposing (..)
import SW.SymbolConverter exposing (..)


type alias EditorSymbol =
    Idable (Selectable (Symbol))


symbolinit : EditorSymbol
symbolinit =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , fontsize = 0
    , size = 1
    , nwcolor = ""
    , pua = ""
    , code = 0
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
    , fontsize = symbol.fontsize
    , size = symbol.size
    , nwcolor = symbol.nwcolor
    , pua = symbol.pua
    , code = symbol.code
    , key = symbol.key
    , nbcolor = symbol.nbcolor
    , selected = False
    , id = id + index + 1
    }


getSymbolEditor : Int -> Int -> Int -> EditorSymbol
getSymbolEditor group fill rotation =
    let
        base =
            group + 255

        key =
            SW.SymbolConverter.key base fill rotation

        pua =
            SW.SymbolConverter.pua key

        code =
            SW.SymbolConverter.codefromkey key

        symbol =
            { x = 0
            , y = 0
            , width = 20
            , height = 20
            , fontsize = 30
            , size = 1
            , nwcolor = "white"
            , pua = pua
            , code = code
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
        List.filter (\symbol -> (intersect seachrectangle (getsymbolRectangle symbol))) symbols
