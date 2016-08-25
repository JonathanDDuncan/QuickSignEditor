module SWEditor.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import SWEditor.Types exposing (..)
import Ports as Ports exposing (..)
import SW.Types exposing (..)
import Mouse exposing (Position)


-- import SubSWEditors.State


init : ( Model, Cmd Msg )
init =
    ( Model "" signinit (Position 200 200) Nothing, Cmd.none )


signinit : Sign
signinit =
    { width = 0
    , height = 0
    , text = ""
    , x = 0
    , y = 0
    , backfill = ""
    , syms = [ symbolinit ]
    , laned = False
    , left = 0
    }


symbolinit : Symbol
symbolinit =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , fontsize = 0
    , nwcolor = ""
    , pua = ""
    , code = 0
    , key = ""
    , nbcolor = ""
    }


update : SWEditor.Types.Msg -> SWEditor.Types.Model -> ( SWEditor.Types.Model, Cmd SWEditor.Types.Msg )
update action ({ position, drag } as model) =
    case action of
        Change newfsw ->
            ( { model | fsw = newfsw, sign = signinit }, Cmd.none )

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        SetSign newsign ->
            ( { model | sign = newsign }, Cmd.none )

        DragStart xy ->
            ( { model | position = position, drag = (Just (Drag xy xy)) }, Cmd.none )

        DragAt xy ->
            ( { model | position = position, drag = (Maybe.map (\{ start } -> Drag start xy) drag) }, Cmd.none )

        DragEnd _ ->
            ( { model | position = (SWEditor.Types.getPosition model), drag = Nothing }, Cmd.none )



-- updateHelp : SWEditor.Types.Msg -> SWEditor.Types.Model -> SWEditor.Types.Model
-- updateHelp msg ({ position, drag } as model) =
--     case msg of
--         DragStart xy ->
--             { model | position = position, drag = (Just (Drag xy xy)) }
--         DragAt xy ->
--             { model | position = position, drag = (Maybe.map (\{ start } -> Drag start xy) drag) }
--         DragEnd _ ->
--             { model | position = (SWEditor.Types.getPosition model), drag = Nothing }
--  SWEditorMsg action ->
--          lift .SWEditorFieldName (\m x -> { m | SWEditorFieldName = x })  SWEditorMsg SWEditor.State.update action model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, receiveSign SetSign ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
