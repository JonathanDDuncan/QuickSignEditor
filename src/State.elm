module State exposing (..)

import Types exposing (..)


init : ( Types.Model, Cmd Types.Msg )
init =
    ( { field = 0
      }
    , Cmd.none
    )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update action model =
    case action of
        NoOp ->
            ( model
            , Cmd.none
            )
