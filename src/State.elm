module State exposing (..)

import Types exposing (..)
import Clock.Types exposing (..)
import Jumbotron.State
import Clock.State


init : ( Types.Model, Cmd Types.Msg )
init =
    ( { jumbotron = fst Jumbotron.State.init
      , clock = fst Clock.State.init
      }
    , Cmd.none
    )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Clock.State.subscriptions model.clock |> Sub.map Clock
        ]


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update action model =
    case action of
        Jumbotron action ->
            ( { model | jumbotron = fst (Jumbotron.State.update action model.jumbotron) }
            , Cmd.none
            )

        Clock action ->
            ( { model | clock = Clock.State.update action model.clock }
            , Cmd.none
            )
