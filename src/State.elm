module State exposing (init, update, subscriptions)

import Types exposing (..)
import PlatformHelpers exposing (lift)
import Overlay.State


-- import Feature.State


init : ( Types.Model, Cmd Types.Msg )
init =
    ( { overlay = fst Overlay.State.init
      }
      -- To initiate feature state
      --  { featureFieldName = fst Feature.State.init
      --  }
    , Cmd.map Overlay (snd Overlay.State.init)
    )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Overlay.State.subscriptions model.overlay |> Sub.map Overlay
        ]



-- To nest subscriptions
-- Sub.batch
--       [ Feature.State.subscriptions model.featureFieldName |> Sub.map FeatureMsg
--       ]


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update action model =
    case action of
        Overlay action ->
            lift .overlay (\m x -> { m | overlay = x }) Overlay Overlay.State.update action model



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model
