module Layout.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Layout.Types exposing (..)


-- import SubLayouts.State


init : ( Model, Cmd Msg )
init =
    ( { field = 0
      }
    , Cmd.none
    )


update : Layout.Types.Msg -> Layout.Types.Model -> ( Layout.Types.Model, Cmd Layout.Types.Msg )
update action model =
    case action of
        LayoutMessage ->
            ( { model | field = 0 }
            , Cmd.none
            )



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model


subscriptions : Layout.Types.Model -> Sub Layout.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubLayout.State.subscriptions model.subLayoutFieldName |> Sub.map SubLayoutMsg
--       ]
