module Jumbotron.State exposing (init, update)

import Jumbotron.Types exposing (..)
import Hello.State


init : ( Model, Cmd Msg )
init =
    ( Model (fst Hello.State.init)
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        Hello action ->
            ( { model | hello = fst (Hello.State.update action model.hello) }
            , Cmd.none
            )
