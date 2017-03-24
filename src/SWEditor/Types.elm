module SWEditor.Types exposing (..)

-- import SubSWEditors.Types

import SW.Types exposing (..)
import Keyboard.Shared exposing (..)


type alias Model =
    { fsw : String
    , sign : Sign
    , xy : Position
    , dragstart : Position
    , dragsign : Sign
    , viewposition : NamedPosition
    , rectanglestart : Position
    , windowresized : Bool
    , editormode : EditorMode
    , containerheight : Int
    , signviewmargin : Int
    , uid : Int
    , undolist : List UndoItem
    , redolist : List UndoItem
    , signviewkeyboard : List (KeyAction Msg)
    }


type alias Offset =
    { offsetx : Int
    , offsety : Int
    }


type alias UndoItem =
    { actionname : String
    , sign : Sign
    }


type Msg
    = ChangeFSW String
    | RequestSign
    | RequestSignfromOtherApp
    | SetSign PortableSign
    | RequestElementPosition String
    | ReceiveElementPosition NamedPosition
    | CenterSign
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
    | DragSymbol Symbol
    | AddSymbol Symbol
    | ReplaceSymbol Symbol
    | AddUndo Bool String Model
    | Undo
    | Redo
    | DeleteSymbols
    | DuplicateSymbols
    | Keyboard KeyboardCommand
    | MoveSymbols Direction Distance
    | SetKeyboardMode KeyboardMode


type alias Distance =
    Int


type Direction
    = Up
    | Down
    | Right
    | Left


type EditorMode
    = Awaiting
    | RectangleSelect
    | AddingSymbol
    | Dragging



-- Plus any other types unique to this SWEditor
-- Plus any library function to run on the types


signViewPosition : Position -> NamedPosition -> Position
signViewPosition position viewposition =
    { x = position.x - viewposition.x, y = position.y - viewposition.y }


withinSignView : Position -> NamedPosition -> Bool
withinSignView signviewposition viewposition =
    signviewposition.x >= 0 && signviewposition.y >= 0 && signviewposition.x <= viewposition.width && signviewposition.y <= viewposition.height
