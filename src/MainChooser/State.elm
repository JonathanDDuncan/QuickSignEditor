module MainChooser.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import MainChooser.Types exposing (..)
import Ports exposing (..)


-- import SubMainChoosers.State


init : ( MainChooser.Types.Model, Cmd MainChooser.Types.Msg )
init =
    ( { choosings = []
      }
      -- To initiate MainChooser state
      --  { MainChooserFieldName = fst MainChooser.State.init
      --  }
    , Cmd.none
    )


update : MainChooser.Types.Msg -> MainChooser.Types.Model -> ( MainChooser.Types.Model, Cmd MainChooser.Types.Msg )
update action model =
    case action of
        MainChooserMessage ->
            ( model
            , Cmd.none
            )

        RequestInitialChoosings ->
            ( model
            , Ports.requestInitialChoosings ""
            )

        ReceiveInitialChoosings choosings ->
            ( {model | choosings = choosings}
            , Cmd.none
            )


--To nest update of MainChooser
--  MainChooserMsg action ->
--          lift .MainChooserFieldName (\m x -> { m | MainChooserFieldName = x })  MainChooserMsg MainChooser.State.update action model


subscriptions : MainChooser.Types.Model -> Sub MainChooser.Types.Msg
subscriptions = 
    Sub.batch
        [ 
            receiveInitialChoosings ReceiveInitialChoosings
        
        ]
    

receiveInitialChoosings

-- To nest subscriptions
-- Sub.batch
--       [ SubMainChooser.State.subscriptions model.subMainChooserFieldName |> Sub.map SubMainChooserMsg
--       ]
