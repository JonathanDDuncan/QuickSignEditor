module Choosing.Types exposing (..)

-- import SubChoosings.Types

import SWEditor.Types exposing (..)
import SW.Types exposing (..)


type alias Model =
    { displaySign : Sign
    , valuestoAdd : List Symbol
    , value : Int
    , offset : Offset
    }


type Msg
    = ChoosingMessage



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
