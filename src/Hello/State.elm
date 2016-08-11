module Hello.State exposing (..)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)

import Hello.Types exposing (..)
 

init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Increment ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )
