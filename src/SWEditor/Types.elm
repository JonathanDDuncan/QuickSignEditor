module SWEditor.Types exposing (..)

-- import SubSWEditors.Types

import SW.Types exposing (..)
import SWEditor.EditorSign exposing (..)


type alias Model =
    { fsw : String
    , sign : EditorSign
    , xy : Position
    , dragstart : Position
    , dragsign : EditorSign
    , viewposition : NamedPosition
    , rectanglestart : Position
    , windowresized : Bool
    , editormode : EditorMode
    , uid : Int
    }


type alias Offset =
    { offsetx : Int
    , offsety : Int
    }


type Msg
    = ChangeFSW String
    | RequestSign
    | RequestSignMakerSign
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


signViewPosition : Position -> NamedPosition -> Position
signViewPosition position viewposition =
    { x = position.x - viewposition.x, y = position.y - viewposition.y }


withinSignView : Position -> NamedPosition -> Bool
withinSignView signviewposition viewposition =
    signviewposition.x >= 0 && signviewposition.y >= 0 && signviewposition.x <= viewposition.width && signviewposition.y <= viewposition.height
