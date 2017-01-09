module Keyboard.Types exposing (..)

-- import SubFeatures.Types

import SWEditor.Types
import SWEditor.EditorSign exposing (..)
import SW.Types exposing (..)
import Keyboard.Extra as KeyboardExtra
import Keyboard.Shared exposing (..)


type alias Model =
    { keyboardlayout : KeyboardLayout
    , keyboarddisplay : KeyboardDisplay
    , keyboardhistory : List String
    , keyboardExtraModel : KeyboardExtra.Model
    , keyList : List Int
    , keyboardmode : KeyboardMode
    }


type Msg
    = KeyClicked Int
    | Display SWEditor.Types.Msg
    | KeyboardExtraMsg KeyboardExtra.Msg


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
