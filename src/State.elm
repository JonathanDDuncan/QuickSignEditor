module State exposing (..)

import Types exposing (..)
import Jumbotron.State
import Clock.State
import JSInterop.State


init : ( Types.Model, Cmd Types.Msg )
init =
    ( { jumbotron = fst Jumbotron.State.init
      , clock = fst Clock.State.init
      , jsInterop = fst JSInterop.State.init
      }
    , Cmd.none
    )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Clock.State.subscriptions model.clock |> Sub.map Clock
        , JSInterop.State.subscriptions model.jsInterop |> Sub.map JSInterop
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

        JSInterop action ->
            ( { model | jsInterop = fst (JSInterop.State.update action model.jsInterop) }
            , Cmd.none
            )
