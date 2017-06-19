module Choosers.KeyboardType exposing (KeyboardType(..))

import Keyboard.Shared exposing (KeyAction, KeyboardCommand, KeyboardMode)


type KeyboardType
    = Keyboard KeyboardCommand
    | SetKeyboardMode KeyboardMode
    | UpdateChooserKeyboards
    | NextKeyboardPage
