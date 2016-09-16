module Keyboard.Types exposing (..)

-- import SubFeatures.Types

import Dict
import SWEditor.Types
import SWEditor.EditorSign exposing (..)
import SW.Types exposing (..)


type alias Model =
    { keyboardlayout : KeyboardLayout
    , keyboarddisplay : KeyboardDisplay
    , keycodedictionary : Dict.Dict Int String
    , keyboardhistory : List String
    }


type Msg
    = FeatureMessage
    | KeyClicked Int
    | Display SWEditor.Types.Msg


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


type alias KeyCodeDictionary =
    { codes : List Int }



-- Plus any other types unique to this Keyboard
-- Plus any library function to run on the types
