module Choosers.KeyboardType exposing (KeyboardType(..))

import Keyboard.Shared exposing (KeyboardCommand)
import Keyboard.KeyboardModeType exposing (KeyboardMode)


type KeyboardType
    = Keyboard KeyboardCommand
    | SetKeyboardMode KeyboardMode
    | UpdateChooserKeyboards
    | NextKeyboardPage
