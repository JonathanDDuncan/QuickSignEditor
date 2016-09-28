module Choosing.Types exposing (..)

-- import SubChoosings.Types

import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Types exposing (..)


type alias Model =
    { displaySign : EditorSign
    , valuestoAdd : List EditorSymbol
    , value : Int
    , offset : Offset
    }


type Msg
    = ChoosingMessage



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
