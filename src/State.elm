module State exposing (..)

import Types exposing (..)
import Jumbotron.State


init : ( Model, Cmd Msg )
init =
    ( { jumbotron = fst Jumbotron.State.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update action model =
    case action of
        Jumbotron action ->
            ( { model | jumbotron = fst (Jumbotron.State.update action model.jumbotron) }
            , Cmd.none
            )
