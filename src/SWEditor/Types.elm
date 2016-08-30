module SWEditor.Types exposing (..)

-- import SubSWEditors.Types

import SW.Types exposing (..)
import Touch.TouchEvents exposing (..)
import SWEditor.Rectangle exposing (..)


type alias Model =
    { fsw : String
    , sign : EditorSign
    , xy : Position
    , drag : Maybe Drag
    , dragstart : Position
    , dragend : Position
    , dragsign : EditorSign
    , viewposition : NamedPosition
    , rectanglestart : Position
    , rectangleend : Position
    , windowresized : Bool
    , editormode : EditorMode
    , uid : Int
    }


type alias Offset =
    { offsetx : Int
    , offsety : Int
    }


type alias EditorSymbol =
    Idable (Selectable (Symbol))


type alias EditorSign =
    { width : Int
    , height : Int
    , text : String
    , x : Int
    , y : Int
    , backfill : String
    , syms : List EditorSymbol
    , laned : Bool
    , left : Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type Msg
    = ChangeFSW String
    | RequestSign
    | SetSign Sign
    | RequestElementPosition String
    | ReceiveElementPosition NamedPosition
    | CenterSign
    | TouchDown Position
    | TouchUp Position
    | MouseDown Position
    | MouseUp Position
    | MouseMove Position
    | UpdateSignViewPosition
    | SelectSymbol Int
    | UnSelectSymbols
    | StartRectangleSelect
    | EndRectangleSelect
    | StartDragging
    | DragSelected
    | EndDragging


type EditorMode
    = Awaiting
    | RectangleSelect
    | Dragging



-- Plus any other types unique to this SWEditor
-- Plus any library function to run on the types


getPosition : Model -> Position
getPosition ({ xy, drag } as model) =
    case drag of
        Nothing ->
            xy

        Just { start, current } ->
            let
                { offsetx, offsety } =
                    getOffset model
            in
                Position
                    (xy.x + offsetx)
                    (xy.y + offsety)


getOffset : Model -> Offset
getOffset { viewposition, drag } =
    case drag of
        Nothing ->
            Offset 0 0

        Just { start, current } ->
            Offset
                (current.x - start.x)
                (current.y - start.y)


getOffset' : Model -> Position -> Offset
getOffset' { drag } pos =
    case drag of
        Nothing ->
            Offset 0 0

        Just { start, current } ->
            Offset
                (pos.x - start.x)
                (pos.y - start.y)


toEditorSign : Sign -> Int -> EditorSign
toEditorSign sign id =
    { width = sign.width
    , height = sign.height
    , text = sign.text
    , x = sign.x
    , y = sign.y
    , backfill = sign.backfill
    , syms = List.indexedMap (toEditorSymbol id) sign.syms
    , laned = sign.laned
    , left = sign.left
    }


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


centerSign model sign =
    let
        width =
            model.viewposition.width

        height =
            model.viewposition.height

        bounding =
            getSignBounding sign

        currentxcenter =
            bounding.x + bounding.width // 2

        currentycenter =
            bounding.y + bounding.height // 2

        desiredxcenter =
            width // 2

        desiredycenter =
            height // 2

        movex =
            desiredxcenter - currentxcenter

        movey =
            desiredycenter - currentycenter

        newsignx =
            model.sign.x + movex

        newsigny =
            model.sign.y + movey
    in
        { sign | x = newsignx, y = newsigny, syms = moveSymbols movex movey sign.syms }


moveSymbols : Int -> Int -> List EditorSymbol -> List EditorSymbol
moveSymbols movex movey symbols =
    List.map (moveSymbol movex movey) symbols


moveSymbol : Int -> Int -> EditorSymbol -> EditorSymbol
moveSymbol movex movey symbol =
    { symbol | x = symbol.x + movex, y = symbol.y + movey }


getSignBounding : EditorSign -> Rect
getSignBounding sign =
    let
        x1 =
            List.foldr (\s -> min s.x) 10000 sign.syms

        y1 =
            List.foldr (\s -> min s.y) 10000 sign.syms

        x2 =
            List.foldr (\s -> max (s.x + s.width)) 0 sign.syms

        y2 =
            List.foldr (\s -> max (s.y + s.height)) 0 sign.syms
    in
        { x = x1, y = y1, width = x2 - x1, height = y2 - y1 }


getlastuid : EditorSign -> Int
getlastuid editorSign =
    case maximumBy .id editorSign.syms of
        Nothing ->
            0

        Just sign ->
            sign.id


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f ls =
    let
        maxBy f x y =
            if (f x) > (f y) then
                x
            else
                y
    in
        case ls of
            l' :: ls' ->
                Just <| List.foldl (maxBy f) l' ls'

            _ ->
                Nothing


signViewPosition : Position -> Model -> Position
signViewPosition position model =
    { x = position.x - model.viewposition.x, y = position.y - model.viewposition.y }


withinSignView : Position -> Model -> Bool
withinSignView signviewposition model =
    signviewposition.x >= 0 && signviewposition.y >= 0 && signviewposition.x <= model.viewposition.width && signviewposition.y <= model.viewposition.height


symbolsUnderPosition : Position -> EditorSign -> List EditorSymbol
symbolsUnderPosition signviewposition sign =
    let
        seachrectangle =
            { x = signviewposition.x, y = signviewposition.y, width = 1, height = 1 }
    in
        List.filter (\symbol -> (intersect seachrectangle (getsymbolRectangle symbol))) sign.syms
