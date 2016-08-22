module Layout.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Layout.Types exposing (..)
import Material
import WindowSize.State
import SWEditor.State
import PlatformHelpers exposing (lift)


init : ( Model, Cmd Msg )
init =
    ( { count =
            0
            -- Boilerplate: Always use this initial Mdl model store.
      , mdl =
            Material.model
      , window =
            fst WindowSize.State.init
      , signbox =
            fst SWEditor.State.init
      , rightdrawer =
            fst drawerinit
      , footerheight =
            100
      , containerHeight =
            800
      , widescreenwidth = 1000
      , mediumscreenwidth = 600
      , rightspacepercentage = 30
      , centerspacepercentage = 40
      , leftspacepercentage = 30
      }
    , Cmd.map Window (snd WindowSize.State.init)
    )


drawerinit : ( DrawerModel, Cmd Msg )
drawerinit =
    ( { active = True
      , showing = False
      , alwaysShowpx = 50
      , fullwidth = 200
      , height = 800
      }
      -- To initiate Drawer state
      --  { DrawerFieldName = fst Drawer.State.init
      --  }
    , Cmd.none
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
            lift .window windowSizeSetter Window WindowSize.State.update action model

        SignBox action ->
            lift .signbox (\m x -> { m | signbox = x }) SignBox SWEditor.State.update action model

        DrawerShow ->
            ( { model | rightdrawer = setdrawerShowing model.rightdrawer True }
            , Cmd.none
            )

        DrawerHide ->
            ( { model | rightdrawer = setdrawerShowing model.rightdrawer False }
            , Cmd.none
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg' ->
            Material.update msg' model


windowSizeSetter =
    (\m x ->
        let
            containerheight =
                getcontainerheight m x
        in
            { m
                | window = x
                , containerHeight = containerheight
                , rightdrawer = setdrawerSize m.rightdrawer containerheight m.window.windowSize.width (getdraweractive m)
                , rightspacepercentage = rightspacePercentage m
                , centerspacepercentage = centerspacePercentage m
                , leftspacepercentage = leftspacePercentage m
            }
    )


rightspacePercentage model =
    if iswidescreen model then
        30
    else if ismediumscreen model then
        50
    else
        100


centerspacePercentage : Model -> Int
centerspacePercentage model =
    if iswidescreen model then
        40
    else if ismediumscreen model then
        50
    else
        100


leftspacePercentage : Model -> Int
leftspacePercentage model =
    if iswidescreen model then
        30
    else if ismediumscreen model then
        100
    else
        100


setdrawerSize : DrawerModel -> Int -> Int -> Bool -> DrawerModel
setdrawerSize model containerheight fullwidth active =
    { model | height = containerheight, fullwidth = fullwidth, active = active }


getdraweractive model =
    if model.window.windowSize.width <= model.widescreenwidth then
        True
    else
        False


setdrawerShowing model showing =
    { model | showing = showing }


getcontainerheight model x =
    x.windowSize.height - model.footerheight



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model


subscriptions : Layout.Types.Model -> Sub Layout.Types.Msg
subscriptions model =
    Sub.batch
        [ WindowSize.State.subscriptions model.window |> Sub.map Window
        , SWEditor.State.subscriptions model.signbox |> Sub.map SignBox
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubLayout.State.subscriptions model.subLayoutFieldName |> Sub.map SubLayoutMsg
--       ]
