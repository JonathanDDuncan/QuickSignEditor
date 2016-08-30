module SWEditor.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Ports as Ports exposing (..)
import Debug
import Update.Extra exposing (..)
import Touch.TouchEvents as Events exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.RectangleSelect exposing (..)
import SWEditor.Drag exposing (..)
import SWEditor.Select exposing (..)


-- import SubSWEditors.State


init : ( Model, Cmd Msg )
init =
    ( { fsw = "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"
      , sign = signinit
      , xy = Position 0 0
      , dragstart = Position 0 0
      , dragsign = signinit
      , rectanglestart = Position 0 0
      , windowresized = False
      , editormode = Awaiting
      , viewposition = { name = "", x = 0, y = 0, width = 0, height = 0 }
      , uid = 0
      }
    , Cmd.none
    )


signinit : EditorSign
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


symbolinit : EditorSymbol
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
    , selected = False
    , id = 0
    }


update : SWEditor.Types.Msg -> SWEditor.Types.Model -> ( SWEditor.Types.Model, Cmd SWEditor.Types.Msg )
update action model =
    case action of
        ChangeFSW newfsw ->
            { model | fsw = newfsw, sign = signinit } ! []

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        SetSign newsign ->
            let
                editorSign =
                    centerSign model (toEditorSign newsign model.uid)

                lastuid =
                    getlastuid editorSign
            in
                { model | sign = editorSign, uid = lastuid } ! [] |> andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, Ports.requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            { model | viewposition = namedposition } ! [] |> andThen update CenterSign

        UpdateSignViewPosition ->
            { model | windowresized = False } ! [] |> andThen update (RequestElementPosition "signView")

        CenterSign ->
            { model | sign = centerSign model model.sign } ! []

        StartDragging ->
            { model | editormode = Dragging, dragstart = model.xy, dragsign = model.sign } ! []

        DragSelected ->
            { model | sign = dragsign model } ! []

        EndDragging ->
            { model | editormode = Awaiting } ! []

        StartRectangleSelect ->
            { model | editormode = RectangleSelect, rectanglestart = model.xy } ! []

        EndRectangleSelect ->
            { model | editormode = Awaiting, sign = rectangleselect model } ! []

        SelectSymbol id ->
            { model | sign = selectSymbolId id model } ! []

        UnSelectSymbols ->
            { model | sign = unselectSymbols model.sign } ! []

        TouchDown position ->
            -- let
            --     dbg =
            --         Debug.log "TouchDown position" position
            --     signviewposition =
            --         Debug.log "signviewposition" (signViewPosition position model)
            --     withinsignview =
            --         Debug.log "signviewposition" (withinSignView signviewposition model)
            --     symbolsunderposition =
            --         Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            -- in
            model ! []

        TouchUp position ->
            -- let
            --     dbg =
            --         Debug.log "TouchUp position" position
            --     signviewposition =
            --         Debug.log "signviewposition" (signViewPosition position model)
            --     withinsignview =
            --         Debug.log "signviewposition" (withinSignView signviewposition model)
            --     symbolsunderposition =
            --         Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            -- in
            model ! []

        MouseDown position ->
            let
                signviewposition =
                    Debug.log "signviewposition" (signViewPosition position model)

                withinsignview =
                    Debug.log "signviewposition" (withinSignView signviewposition model)

                symbolsunderposition =
                    Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)

                howmanysymbolsunderposition =
                    List.length symbolsunderposition

                selectedsymbols =
                    countselectedsymbols symbolsunderposition

                firstsymbol =
                    List.head symbolsunderposition

                firstsymbolid =
                    symbolId firstsymbol
            in
                model
                    ! []
                    |> filter (selectedsymbols > 0 && withinsignview)
                        (andThen update (StartDragging))
                    |> filter (howmanysymbolsunderposition == 1 && withinsignview)
                        (andThen update (SelectSymbol firstsymbolid))
                    |> filter (howmanysymbolsunderposition == 0 && withinsignview)
                        (andThen update (UnSelectSymbols)
                            >> andThen update StartRectangleSelect
                        )

        MouseUp position ->
            let
                signviewposition =
                    Debug.log "signviewposition" (signViewPosition position model)

                withinsignview =
                    Debug.log "signviewposition" (withinSignView signviewposition model)

                symbolsunderposition =
                    Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            in
                model
                    ! []
                    |> filter (model.editormode == RectangleSelect)
                        (andThen update EndRectangleSelect)
                    |> filter (model.editormode == Dragging)
                        (andThen update EndDragging)

        MouseMove position ->
            let
                signviewposition =
                    signViewPosition position model

                withinsignview =
                    withinSignView signviewposition model

                symbolsunderposition =
                    symbolsUnderPosition signviewposition model.sign
            in
                { model | xy = signviewposition }
                    ! []
                    |> filter (model.windowresized)
                        (andThen update UpdateSignViewPosition)
                    |> filter (model.editormode == Dragging)
                        (andThen update DragSelected)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.downs MouseDown
        , Events.moves MouseMove
        , Events.ups MouseUp
        , Events.touchdowns TouchDown
        , Events.touchmoves MouseMove
        , Events.touchups TouchUp
        , receiveSign SetSign
        , receiveElementPosition ReceiveElementPosition
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
