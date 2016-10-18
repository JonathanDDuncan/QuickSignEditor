module Choosing.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Choosing.Types exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.EditorSign as EditorSign exposing (..)


-- import SubChoosings.State


init : String -> Int -> Int -> ( Choosing.Types.Model, Cmd Choosing.Types.Msg )
init key x y =
    ( { displaySign = EditorSign.signinit
      , valuestoAdd = []
      , value = key
      , offset = Offset x y
      }
      -- To initiate feature state
      --  { featureFieldName = fst Choosing.State.init
      --  }
    , Cmd.none
    )


update : Choosing.Types.Msg -> Choosing.Types.Model -> ( Choosing.Types.Model, Cmd Choosing.Types.Msg )
update action model =
    case action of
        ChoosingMessage ->
            ( model
            , Cmd.none
            )

        Display msg ->
            ( model
            , Cmd.none
            )



--To nest update of feature
--  ChoosingMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  ChoosingMsg Choosing.State.update action model


subscriptions : Choosing.Types.Model -> Sub Choosing.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubChoosing.State.subscriptions model.subfeatureFieldName |> Sub.map SubChoosingMsg
--       ]
