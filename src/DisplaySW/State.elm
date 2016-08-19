module DisplaySW.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import DisplaySW.Types exposing (..)


-- import SubDisplaySWs.State


init : ( DisplaySW.Types.Model, Cmd DisplaySW.Types.Msg )
init =
    ( { field = 0
      }
      -- To initiate DisplaySW state
      --  { DisplaySWFieldName = fst DisplaySW.State.init
      --  }
    , Cmd.none
    )


update : DisplaySW.Types.Msg -> DisplaySW.Types.Model -> ( DisplaySW.Types.Model, Cmd DisplaySW.Types.Msg )
update action model =
    case action of
        DisplaySWMessage ->
            ( { model | field = 0 }
            , Cmd.none
            )



--To nest update of DisplaySW
--  DisplaySWMsg action ->
--          lift .DisplaySWFieldName (\m x -> { m | DisplaySWFieldName = x })  DisplaySWMsg DisplaySW.State.update action model


subscriptions : DisplaySW.Types.Model -> Sub DisplaySW.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubDisplaySW.State.subscriptions model.subDisplaySWFieldName |> Sub.map SubDisplaySWMsg
--       ]
