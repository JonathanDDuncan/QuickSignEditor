module Overlay.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components

import PlatformHelpers exposing (..)
import Overlay.Types exposing (..)
import Layout.Types exposing (..)
import Layout.State
import Ports exposing (..)
import SWEditor.EditorSign exposing (..)


-- import SubOverlays.State


init : ( Overlay.Types.Model, Cmd Overlay.Types.Msg )
init =
    ( { layout = fst Layout.State.init
      , show = True
      }
    , Cmd.map Layout (snd Layout.State.init)
    )


update : Overlay.Types.Msg -> Overlay.Types.Model -> ( Overlay.Types.Model, Cmd Overlay.Types.Msg )
update msg model =
    case msg of
        Hide ->
            ( { model | show = False }, Cmd.none )

        Show ->
            ( { model | show = True }, Ports.requestSignfromOtherAppDelayed "" )

        Layout action ->
            layoutactions action model


layoutactions : Layout.Types.Msg -> Overlay.Types.Model -> ( Overlay.Types.Model, Cmd Overlay.Types.Msg )
layoutactions action model =
    case action of
        HideOverlay ->
            ( { model | show = False }, Cmd.none )

        ShareFsw ->
            let
                fsw =
                    SWEditor.EditorSign.getFsw model.layout.signbox.sign
            in
                ( { model | show = False }, Ports.shareFsw fsw )

        PleaseShareFsw msg ->
            let
                fsw =
                   SWEditor.EditorSign.getFsw model.layout.signbox.sign
            in
                ( model, Ports.shareFsw fsw )

        _ ->
            lift
                .layout
                (\m x -> { m | layout = x })
                Layout
                Layout.State.update
                action
                model



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model


subscriptions : Overlay.Types.Model -> Sub Overlay.Types.Msg
subscriptions model =
    Sub.batch
        [ Layout.State.subscriptions model.layout |> Sub.map Layout
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubOverlay.State.subscriptions model.subOverlayFieldName |> Sub.map SubOverlayMsg
--       ]
