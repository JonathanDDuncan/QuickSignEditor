module SWEditor.Types exposing (..)

-- import SubSWEditors.Types

import SW.Types exposing (..)


type alias Model =
    { fsw : String
    , sign : Sign
    }


type Msg
    = Change String
    | RequestSign
    | SetSign Sign



-- Plus any other types unique to this SWEditor
-- Plus any library function to run on the types
