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


subscriptions : Sub Clock.Types.Msg -> Sub Types.Msg
subscriptions =
    Sub.map Clock


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
