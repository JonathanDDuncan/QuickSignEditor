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
    = Change String
    | RequestSign
    | SetSign Sign
    | RequestElementPosition String
    | ReceiveElementPosition NamedPosition
    | SelectSignsInRectangle
    | DragAt Position
    | DragEnd Position
    | SymbolMouseDown Int
    | CenterSign
    | TouchDown Position
    | TouchUp Position
    | MouseDown Position
    | MouseUp Position
    | MouseMove Position
    | UpdateSignViewPosition
    | SelectSymbol Int
    | UnSelectSymbols
    | RectangleSelectMode
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


rectangleSelect : Model -> Rect
rectangleSelect model =
    let
        x1 =
            min model.rectanglestart.x model.rectangleend.x

        x2 =
            max model.rectanglestart.x model.rectangleend.x

        y1 =
            min model.rectanglestart.y model.rectangleend.y

        y2 =
            max model.rectanglestart.y model.rectangleend.y
    in
        { x = x1
        , y = y1
        , width = x2 - x1
        , height = y2 - y1
        }
