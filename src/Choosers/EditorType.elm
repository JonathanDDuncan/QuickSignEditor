module Choosers.EditorType exposing (Editor(..))

import SW.Pua exposing (Key)
import Choosers.ChooserItemType exposing (ChooserItem)


type Editor
    = SelectedColumn Int
    | Clicked String
    | GroupSelected ChooserItem
    | AddSymbol Key
    | DragSymbol Key
    | ReplaceSymbol Key
