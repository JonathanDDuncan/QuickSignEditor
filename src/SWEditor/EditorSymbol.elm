module SWEditor.EditorSymbol exposing (..)

import SW.Types exposing (..)
import SWEditor.Rectangle exposing (..)


type alias EditorSymbol =
    Idable (Selectable (Symbol))


toEditorSymbol : Int -> Int -> Symbol -> EditorSymbol
toEditorSymbol id index symbol =
    { x = symbol.x
    , y = symbol.y
    , width = symbol.width
    , height = symbol.height
    , fontsize = symbol.fontsize
    , nwcolor = symbol.nwcolor
    , pua = symbol.pua
    , code = symbol.code
    , key = symbol.key
    , nbcolor = symbol.nbcolor
    , selected = False
    , id = id + index + 1
    }


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
