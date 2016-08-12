module JSInterop.State exposing (..)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)

import JSInterop.Types exposing (..)
import Ports exposing (..)

 
init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    suggestions Suggest 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newWord ->
            ( Model (Debug.log "Change newWord" newWord) [], Cmd.none )

        Check ->
            ( model, check <| Debug.log "Check" model.word )

        Suggest newSuggestions ->
            ( Model model.word <| Debug.log "Suggest newSuggestions" newSuggestions, Cmd.none )
