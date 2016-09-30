module MainChooser.Types exposing (..)

-- import SubMainChoosers.Types

import Choosing.Types as Choosing


type alias Model =
    List Choosing.Model


type Msg
    = MainChooserMessage
    | RequestInitialChoosings
    | ReceiveInitialChoosings (List Choosing.ImportModel)
    | Choosing Choosing.Msg



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
