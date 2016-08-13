module Feature.State exposing (init, update, liftMe, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)

import PlatformHelpers exposing (..)
import Feature.Types exposing (..)
 
-- import SubFeatures.State

init : ( Feature.Types.Model, Cmd Feature.Types.Msg )
init =
    ( { field = 0
      }
      -- To initiate feature state
      --  { featureFieldName = fst Feature.State.init
      --  }
    , Cmd.none
    )


update : Feature.Types.Msg -> Feature.Types.Model -> ( Feature.Types.Model, Cmd Feature.Types.Msg )
update action model =
    case action of
        FeatureMessage ->
            ( { model | field = 0 }
            , Cmd.none
            )



--To nest update of feature
--  SubFeatureMsg action ->
--           SubFeature.State.liftMe .subFeatureFieldName (\m x -> { m | subFeatureFieldName = x })  SubFeatureMsg action model
-- where
-- SubFeature.State.liftMe is
-- liftMe get set fwd =
--     lift get set fwd update


liftMe get set fwd =
    lift get set fwd update


subscriptions : Feature.Types.Model -> Sub Feature.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubFeature.State.subscriptions model.subfeatureFieldName |> Sub.map SubFeatureMsg
--       ]
