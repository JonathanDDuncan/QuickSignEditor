module Keyboard.Types exposing (..)

-- import SubFeatures.Types

import Dict


type alias Model =
    { keyboardlayout : KeyboardLayout
    , keycodedictionary : Dict.Dict Int String
    }


type Msg
    = FeatureMessage


type alias KeyboardLayout =
    { name : String
    , codes : List Int
    }


type alias KeyCodeDictionary =
    { codes : List Int }



-- Plus any other types unique to this Keyboard
-- Plus any library function to run on the types
