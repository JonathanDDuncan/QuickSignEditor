module Keyboard.Types exposing (..)

-- import SubFeatures.Types

import Dict
import SWEditor.Types


type alias Model =
    { keyboardlayout : KeyboardLayout
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


type alias Key =
    { display : String
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
