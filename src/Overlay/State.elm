module Overlay.State exposing (init, update, subscriptions)

import PlatformHelpers exposing (..)
import Overlay.Types exposing (Model, Msg(..))
import Layout.Types exposing (Model, Msg(HideOverlay, ShareFsw, PleaseShareFsw))
import Layout.State
import Ports exposing (requestSignfromOtherAppDelayed, hideOverlay, shareFsw)
import SW.FSW exposing (..)


-- import SubOverlays.State


init : ( Overlay.Types.Model, Cmd Overlay.Types.Msg )
init =
    ( { layout = Tuple.first Layout.State.init
      , show = True
      }
    , Cmd.map Layout (Tuple.second Layout.State.init)
    )


update : Overlay.Types.Msg -> Overlay.Types.Model -> ( Overlay.Types.Model, Cmd Overlay.Types.Msg )
update msg model =
    case msg of
        Hide ->
            ( { model | show = False }, Cmd.none )

        Show ->
            ( { model | show = True }, requestSignfromOtherAppDelayed "" )

        Layout action ->
            layoutactions action model


layoutactions : Layout.Types.Msg -> Overlay.Types.Model -> ( Overlay.Types.Model, Cmd Overlay.Types.Msg )
layoutactions action model =
    case action of
        HideOverlay ->
            ( { model | show = False }, hideOverlay "" )

        ShareFsw ->
            let
                fsw =
                    getFsw model.layout.signbox.sign
            in
                ( { model | show = False }, Cmd.batch [ shareFsw fsw, hideOverlay "" ] )

        PleaseShareFsw msg ->
            let
                fsw =
                    getFsw model.layout.signbox.sign
            in
                ( model, shareFsw fsw )

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
