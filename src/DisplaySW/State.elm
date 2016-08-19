module DisplaySW.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import DisplaySW.Types exposing (..)
import Ports as Ports exposing (..)
import Debug


-- import SubDisplaySWs.State


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )


update : DisplaySW.Types.Msg -> DisplaySW.Types.Model -> ( DisplaySW.Types.Model, Cmd DisplaySW.Types.Msg )
update action model =
    case action of
        Change newWord ->
            ( Model newWord [], Cmd.none )

        Check ->
            ( model, Ports.check <| Debug.log "Word to Check" model.word )

        Suggest newSuggestions ->
            ( Model model.word <| Debug.log "New suggestions" newSuggestions, Cmd.none )



--To nest update of DisplaySW
--  DisplaySWMsg action ->
--          lift .DisplaySWFieldName (\m x -> { m | DisplaySWFieldName = x })  DisplaySWMsg DisplaySW.State.update action model


subscriptions : Model -> Sub Msg
subscriptions model =
    suggestions Suggest



-- To nest subscriptions
-- Sub.batch
--       [ SubDisplaySW.State.subscriptions model.subDisplaySWFieldName |> Sub.map SubDisplaySWMsg
--       ]
