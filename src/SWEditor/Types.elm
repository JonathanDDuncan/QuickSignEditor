module SWEditor.Types exposing (..)

-- import SubSWEditors.Types

import SW.Types exposing (..)
import Mouse exposing (Position)


type alias Model =
    { fsw : String
    , sign : EditorSign
    , position : Position
    , drag : Maybe Drag
    , justdragged : Bool
    , rectanglestart : Position
    , rectangleend : Position
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
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | DrawRectangleStart Position
    | DrawRectangleAt Position
    | DrawRectangleEnd Position
    | Select Int



-- Plus any other types unique to this SWEditor
-- Plus any library function to run on the types


getPosition : Model -> Position
getPosition ({ position, drag } as model) =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            let
                { offsetx, offsety } =
                    getOffset model
            in
                Position
                    (position.x + offsetx)
                    (position.y + offsety)


getOffset : Model -> Offset
getOffset { position, drag } =
    case drag of
        Nothing ->
            Offset 0 0

        Just { start, current } ->
            Offset
                (current.x - start.x)
                (current.y - start.y)


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
