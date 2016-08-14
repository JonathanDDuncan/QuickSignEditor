module State exposing (init, update, subscriptions)

import Types exposing (..)


-- import Feature.State


init : ( Types.Model, Cmd Types.Msg )
init =
    ( { field = 0
      }
      -- To initiate feature state
      --  { featureFieldName = fst Feature.State.init
      --  }
    , Cmd.none
    )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ Feature.State.subscriptions model.featureFieldName |> Sub.map FeatureMsg
--       ]


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update action model =
    case action of
        NoOp ->
            ( model
            , Cmd.none
            )



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model
