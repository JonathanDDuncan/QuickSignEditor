module SWEditor.Types
    exposing
        ( Model
        , Msg(..)
        , Direction(..)
        , EditorMode(..)
        , Distance
        , Offset
        , UndoItem
        , signViewPosition
        , withinSignView
        )

import SW.Types exposing (Position, NamedPosition)
import SW.PortableSign exposing (PortableSign)
import SW.Sign exposing (Sign)
import SW.Symbol exposing (Symbol)
import Keyboard.Shared exposing (KeyAction, KeyboardCommand, KeyboardMode)


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
      -- | RequestSign
      -- | RequestSignfromOtherApp
    | SetSign PortableSign
    | UpdateSignViewPosition
    | RequestElementPosition String
    | ReceiveElementPosition NamedPosition
    | CenterSign
    | Keyboard KeyboardCommand
    | SetKeyboardMode KeyboardMode
    | MouseDown Position
    | MouseUp Position
    | MouseMove Position
    | StartRectangleSelect
    | EndRectangleSelect
    | StartDragging
    | DragSelected
    | EndDragging
    | DragSymbol Symbol
    | SelectSymbol Int
    | UnSelectSymbols
    | AddSymbol Symbol
    | ReplaceSymbol Symbol
    | DeleteSymbols
    | DuplicateSymbols
    | MoveSymbols Direction Distance
    | SizeIncreaseSymbols
    | SizeDecreaseSymbols
    | AddUndo Bool String Model
    | Undo
    | Redo


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


signViewPosition : Position -> NamedPosition -> Int -> Position
signViewPosition position viewposition margin =
    { x = position.x - viewposition.x + margin, y = position.y - viewposition.y + margin }


withinSignView : Position -> NamedPosition -> Bool
withinSignView signviewposition viewposition =
    signviewposition.x >= 0 && signviewposition.y >= 0 && signviewposition.x <= viewposition.width && signviewposition.y <= viewposition.height
