module Clock.State exposing (..)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)

import Clock.Types exposing (..)
import Time exposing (Time, second)


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick newTime ->
            newTime
