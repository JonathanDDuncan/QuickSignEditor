module Layout.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Layout.Types exposing (..)
import Material
import WindowSize.State
import PlatformHelpers exposing (lift)


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      , mdl =
            Material.model
      , window =
            fst WindowSize.State.init
      , footerheight =
            100
            -- Boilerplate: Always use this initial Mdl model store.
      }
    , Cmd.map Window (snd WindowSize.State.init)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        HideOverlay ->
            ( model
            , Cmd.none
            )

        Window action ->
            lift .window (\m x -> { m | window = x }) Window WindowSize.State.update action model

        -- Boilerplate: Mdl action handler.
        Mdl msg' ->
            Material.update msg' model



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model


subscriptions : Layout.Types.Model -> Sub Layout.Types.Msg
subscriptions model =
    Sub.batch
        [ WindowSize.State.subscriptions model.window |> Sub.map Window
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubLayout.State.subscriptions model.subLayoutFieldName |> Sub.map SubLayoutMsg
--       ]
