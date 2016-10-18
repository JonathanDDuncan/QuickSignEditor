module MainChooser.Types exposing (..)

-- import SubMainChoosers.Types

import Choosing.Types as Choosing


type alias Model =
    { choosings : List Choosing.Model
    , clicked : String
    }


type Msg
    = MainChooserMessage
    | RequestInitialChoosings
    | ReceiveInitialChoosings (List Choosing.ImportModel)
    | Choosing Choosing.Msg
    | Clicked String



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
