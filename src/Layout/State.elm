module Layout.State exposing (init, update, subscriptions, Model, Msg(..), DrawerModel, iswidescreen, ismediumscreen)

import Material
import WindowSize.State
import WindowSize.Types
import SWEditor.State
import PlatformHelpers exposing (lift)
import Keyboard.State
import Choosers.State
import Ports exposing (pleaseShareFsw)
import Update.Extra
import SWEditor.Types
import Keyboard.Types
import Choosers.Types


type alias Model =
    { count : Int
    , mdl : Material.Model
    , window : WindowSize.Types.Model
    , signbox : SWEditor.Types.Model
    , keyboard : Keyboard.Types.Model
    , mainchooser : Choosers.Types.Model
    , rightdrawer : DrawerModel
    , footerheight : Int
    , containerHeight : Int
    , widescreenwidth : Int
    , mediumscreenwidth : Int
    , rightspacepercentage : Int
    , centerspacepercentage : Int
    , leftspacepercentage : Int
    , rightspacemarginleftpercentage : Int
    , centerspacemarginleftpercentage : Int
    , drawerwidth : Int
    }


type Msg
    = Increase
    | Reset
    | HideOverlay
    | ShareFsw
    | PleaseShareFsw String
    | Window WindowSize.Types.Msg
    | SWEditor SWEditor.Types.Msg
    | Keyboard Keyboard.Types.Msg
    | Choosers Choosers.Types.Msg
    | DrawerShow
    | DrawerHide
    | Mdl (Material.Msg Msg)


type alias DrawerModel =
    { active : Bool
    , showing : Bool
    , fullwidth : Int
    , alwaysShowpx : Int
    , height : Int
    }


leftspacepercentage : number
leftspacepercentage =
    20


centerspacepercentage : number
centerspacepercentage =
    40


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      , mdl = Material.model
      , window = Tuple.first WindowSize.State.init
      , signbox = Tuple.first SWEditor.State.init
      , keyboard = Tuple.first Keyboard.State.init
      , mainchooser = Tuple.first Choosers.State.init
      , rightdrawer = Tuple.first drawerinit
      , footerheight = 176
      , containerHeight = 800
      , widescreenwidth = 1000
      , mediumscreenwidth = 600
      , rightspacepercentage = 40
      , centerspacepercentage = centerspacepercentage
      , leftspacepercentage = leftspacepercentage
      , centerspacemarginleftpercentage = leftspacepercentage
      , rightspacemarginleftpercentage = leftspacepercentage + centerspacepercentage
      , drawerwidth = 0
      }
    , Cmd.batch
        [ Cmd.map Window (Tuple.second WindowSize.State.init)
        , Cmd.map Choosers (Tuple.second Choosers.State.init)
        ]
    )


drawerinit : ( DrawerModel, Cmd Msg )
drawerinit =
    ( { active = True
      , showing = False
      , alwaysShowpx = 50
      , fullwidth = 200
      , height = 800
      }
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

        ShareFsw ->
            ( model
            , Cmd.none
            )
                |> Update.Extra.andThen update HideOverlay

        PleaseShareFsw _ ->
            ( model
            , Cmd.none
            )

        Window action ->
            lift .window windowSizeSetter Window WindowSize.State.update action model

        SWEditor action ->
            lift .signbox (\m x -> { m | signbox = x }) SWEditor SWEditor.State.update action model

        Keyboard action ->
            lift .keyboard (\m x -> { m | keyboard = x }) Keyboard Keyboard.State.update action model

        Choosers action ->
            lift .mainchooser (\m x -> { m | mainchooser = x }) Choosers Choosers.State.update action model

        DrawerShow ->
            ( { model | rightdrawer = setdrawerShowing model.rightdrawer True }
            , Cmd.none
            )

        DrawerHide ->
            ( { model | rightdrawer = setdrawerShowing model.rightdrawer False }
            , Cmd.none
            )

        Mdl msg_ ->
            Material.update Mdl msg_ model


windowSizeSetter : Model -> WindowSize.Types.Model -> Model
windowSizeSetter =
    \m x ->
        let
            containerheight =
                getcontainerheight m x.windowSize.height

            rdrawer =
                setdrawerSize m.rightdrawer containerheight windowwidth (getdraweractive m)

            windowwidth =
                x.windowSize.width

            sbox =
                m.signbox
        in
            { m
                | window = x
                , containerHeight = containerheight
                , rightdrawer = rdrawer
                , rightspacepercentage = rightspacePercentage m windowwidth
                , rightspacemarginleftpercentage = rightspaceMarginLeftPercentage m windowwidth
                , centerspacepercentage = centerspacePercentage m windowwidth
                , centerspacemarginleftpercentage = centerspaceMarginLeftPercentage m windowwidth
                , leftspacepercentage = leftspacePercentage m windowwidth
                , drawerwidth = drawerWidth rdrawer.active rdrawer.showing rdrawer.fullwidth rdrawer.alwaysShowpx
                , signbox = { sbox | containerheight = containerheight, windowresized = True }
            }


rightspaceMarginLeftPercentage : Model -> Int -> Int
rightspaceMarginLeftPercentage model windowwidth =
    if iswidescreenexplicit windowwidth model.widescreenwidth then
        60
    else if ismediumscreenexplicit windowwidth model.mediumscreenwidth then
        50
    else
        0


rightspacePercentage : Model -> Int -> Int
rightspacePercentage model windowwidth =
    if iswidescreenexplicit windowwidth model.widescreenwidth then
        40
    else if ismediumscreenexplicit windowwidth model.mediumscreenwidth then
        50
    else
        100


centerspacePercentage : Model -> Int -> Int
centerspacePercentage model windowwidth =
    if iswidescreenexplicit windowwidth model.widescreenwidth then
        40
    else if ismediumscreenexplicit windowwidth model.mediumscreenwidth then
        50
    else
        100


centerspaceMarginLeftPercentage : Model -> Int -> Int
centerspaceMarginLeftPercentage model windowwidth =
    if iswidescreenexplicit windowwidth model.widescreenwidth then
        20
    else if ismediumscreenexplicit windowwidth model.mediumscreenwidth then
        0
    else
        0


leftspacePercentage : Model -> Int -> Int
leftspacePercentage model windowwidth =
    if iswidescreenexplicit windowwidth model.widescreenwidth then
        20
    else if ismediumscreenexplicit windowwidth model.mediumscreenwidth then
        100
    else
        100


setdrawerSize : DrawerModel -> Int -> Int -> Bool -> DrawerModel
setdrawerSize model containerheight fullwidth active =
    { model | height = containerheight, fullwidth = fullwidth, active = active }


drawerWidth : Bool -> Bool -> Int -> Int -> Int
drawerWidth active showing fullwidth alwaysShowpx =
    if active then
        if showing then
            fullwidth
        else
            alwaysShowpx
    else
        0


getdraweractive : Model -> Bool
getdraweractive model =
    if model.window.windowSize.width <= model.widescreenwidth then
        True
    else
        False


setdrawerShowing : DrawerModel -> Bool -> DrawerModel
setdrawerShowing model showing =
    { model | showing = showing }


getcontainerheight : Model -> Int -> Int
getcontainerheight model windowheight =
    windowheight - model.footerheight


iswidescreen : Model -> Bool
iswidescreen model =
    iswidescreenexplicit model.window.windowSize.width model.widescreenwidth


iswidescreenexplicit : Int -> Int -> Bool
iswidescreenexplicit width widescreenwidth =
    width > widescreenwidth


ismediumscreen : Model -> Bool
ismediumscreen model =
    ismediumscreenexplicit model.window.windowSize.width model.mediumscreenwidth


ismediumscreenexplicit : Int -> Int -> Bool
ismediumscreenexplicit width mediumscreenwidth =
    width > mediumscreenwidth



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WindowSize.State.subscriptions |> Sub.map Window
        , SWEditor.State.subscriptions model.signbox |> Sub.map SWEditor
        , Keyboard.State.subscriptions |> Sub.map Keyboard
        , Choosers.State.subscriptions |> Sub.map Choosers
        , pleaseShareFsw PleaseShareFsw
        ]
