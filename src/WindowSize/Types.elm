module WindowSize.Types exposing (Model, Msg(..))

import Window


type alias Model =
    { windowSize : Window.Size
    }


type Msg
    = Resize Window.Size
    | Idle



-- Plus any other types unique to this Window
-- Plus any library function to run on the types
