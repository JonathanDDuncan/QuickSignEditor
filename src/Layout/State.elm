module Layout.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Layout.Types exposing (..)
import Material
import WindowSize.State
import Drawer.State
import PlatformHelpers exposing (lift)


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      , mdl =
            Material.model
      , window =
            fst WindowSize.State.init
      , rightdrawer =
            fst Drawer.State.init
      , footerheight =
            100
      , containerHeight =
            800
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
            lift .window (\m x -> { m | window = x, containerHeight = getcontainerheight m, rightdrawer = (\m x -> { m | height = x }) m.rightdrawer (getcontainerheight m) }) Window WindowSize.State.update action model

        RightDrawer action ->
            lift .rightdrawer (\m x -> { m | rightdrawer = x }) RightDrawer Drawer.State.update action model

        -- Boilerplate: Mdl action handler.
        Mdl msg' ->
            Material.update msg' model


getcontainerheight : Model -> Int
getcontainerheight model =
    model.window.windowSize.height - model.footerheight



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
