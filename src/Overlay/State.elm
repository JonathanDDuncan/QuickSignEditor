module Overlay.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components

import PlatformHelpers exposing (..)
import Overlay.Types exposing (..)
import Layout.State


-- import SubOverlays.State


init : ( Model, Cmd Msg )
init =
    ( { layout =
            fst Layout.State.init
      , show = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Layout msg ->
        --     ( { model | layout = Layout.State.update msg model.layout }, Cmd.none )
        Hide ->
            ( { model | show = False }, Cmd.none )

        Layout action ->
            lift .layout (\m x -> { m | layout = x }) Layout Layout.State.update action model



--To nest update of feature
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Feature.State.update action model


subscriptions : Overlay.Types.Model -> Sub Overlay.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubOverlay.State.subscriptions model.subOverlayFieldName |> Sub.map SubOverlayMsg
--       ]
