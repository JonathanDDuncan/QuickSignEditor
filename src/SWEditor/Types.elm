module SWEditor.Types exposing (..)

-- import SubSWEditors.Types

import SW.Types exposing (..)
import Mouse exposing (Position)


type alias Model =
    { fsw : String
    , sign : Sign
    , position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type Msg
    = Change String
    | RequestSign
    | SetSign Sign
    | DragStart Position
    | DragAt Position
    | DragEnd Position



-- Plus any other types unique to this SWEditor
-- Plus any library function to run on the types


getPosition : Model -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
