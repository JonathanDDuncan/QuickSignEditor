module DisplaySW.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import DisplaySW.Types exposing (..)
import Ports as Ports exposing (..)
import SW.Types exposing (..)


-- import SubDisplaySWs.State


init : ( Model, Cmd Msg )
init =
    ( Model "" signinit, Cmd.none )


signinit : Sign
signinit =
    { width = 0
    , height = 0
    , text = ""
    , x = 0
    , y = 0
    , backfill = ""
    , syms = [ symbolinit ]
    , laned = False
    , left = 0
    }


symbolinit : Symbol
symbolinit =
    { x = 0
    , y = 0
    , fontsize = 0
    , nwcolor = ""
    , pua = ""
    , code = 0
    , key = ""
    , nbcolor = ""
    }


update : DisplaySW.Types.Msg -> DisplaySW.Types.Model -> ( DisplaySW.Types.Model, Cmd DisplaySW.Types.Msg )
update action model =
    case action of
        Change newWord ->
            ( Model newWord signinit, Cmd.none )

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        SetSign newSuggestions ->
            ( Model model.fsw newSuggestions, Cmd.none )



--To nest update of DisplaySW
--  DisplaySWMsg action ->
--          lift .DisplaySWFieldName (\m x -> { m | DisplaySWFieldName = x })  DisplaySWMsg DisplaySW.State.update action model


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveSign SetSign



-- To nest subscriptions
-- Sub.batch
--       [ SubDisplaySW.State.subscriptions model.subDisplaySWFieldName |> Sub.map SubDisplaySWMsg
--       ]
