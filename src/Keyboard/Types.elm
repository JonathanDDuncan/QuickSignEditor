module Keyboard.Types exposing (..)

-- import SubFeatures.Types

import SWEditor.EditorSign exposing (..)
import SW.Types exposing (..)
import Keyboard.Extra
import Keyboard.Shared exposing (..)
import SWEditor.Types exposing (Msg)
import Choosers.Types exposing (Msg)


type alias Model =
    { keyboardlayout : KeyboardLayout
    , keyboarddisplay : KeyboardDisplay
    , keyboardhistory : List String
    , keyboardExtraModel : Keyboard.Extra.State
    , keyList : List Int
    , keyboardmode : KeyboardMode
    }


type Msg
    = KeyClicked Int
    | DisplaySignView SWEditor.Types.Msg
    | DisplayChoosers Choosers.Types.Msg
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | SetKeyboardMode Int


type alias KeyboardLayout =
    { name : String
    , keys : List Key
    }


type alias KeyboardDisplay =
    { name : String
    , keys : List KeyDisplay
    }


type alias FingerSpellingTemplate =
    List FingerSpellingLetterTemplate


type alias FingerSpellingLetterTemplate =
    { letter : String
    , fsw : String
    , sign : Maybe Sign
    }


type alias Key =
    { display : String
    , code : Int
    , keypress : Keypress
    , keyId : Int
    }


type alias KeyDisplay =
    { sign : EditorSign
    , code : Int
    , keypress : Keypress
    }


type Keypress
    = None
    | Insert
    | Delete
    | PgUp
    | PgDown
    | Home
    | End



-- Plus any other types unique to this Keyboard
-- Plus any library function to run on the types
