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
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import SW.SymbolConverter exposing (..)
import Dict exposing (..)


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


update : SWEditor.Types.Msg -> SWEditor.Types.Model -> ( SWEditor.Types.Model, Cmd SWEditor.Types.Msg )
update action model =
    case action of
        ChangeFSW newfsw ->
            { model | fsw = newfsw, sign = signinit } ! []

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        RequestSignfromOtherApp ->
            ( model, Ports.requestSignfromOtherApp "" )

        SetSign newsign ->
            let
                editorSign =
                    toEditorSign newsign model.uid

                lastuid =
                    getlastuid <| editorSign
            in
                { model | sign = editorSign, uid = lastuid } ! [] |> andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, Ports.requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            { model | viewposition = namedposition } ! [] |> andThen update CenterSign

        UpdateSignViewPosition ->
            { model | windowresized = False } ! [] |> andThen update (RequestElementPosition "signView")

        CenterSign ->
            { model | sign = centerSignViewposition model.viewposition model.sign } ! []

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
            { model | sign = selectSymbolId id model } ! [] |> andThen update (StartDragging)

        UnSelectSymbols ->
            { model | sign = unselectSymbols model.sign } ! []

        TouchDown position ->
            -- let
            --     dbg =
            --         Debug.log "TouchDown position" position
            --     signviewposition =
            --         Debug.log "signviewposition" (signViewPosition position model.viewposition)
            --     withinsignview =
            --         Debug.log "signviewposition" (withinSignView signviewposition model.viewposition)
            --     symbolsunderposition =
            --         Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            -- in
            model ! []

        TouchUp position ->
            -- let
            --     dbg =
            --         Debug.log "TouchUp position" position
            --     signviewposition =
            --         Debug.log "signviewposition" (signViewPosition position model.viewposition)
            --     withinsignview =
            --         Debug.log "signviewposition" (withinSignView signviewposition model.viewposition)
            --     symbolsunderposition =
            --         Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            -- in
            model ! []

        MouseDown position ->
            let
                signviewposition =
                    (signViewPosition position model.viewposition)

                withinsignview =
                    (withinSignView signviewposition model.viewposition)

                symbolsunderposition =
                    (symbolsUnderPosition signviewposition model.sign.syms)

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
                    |> Update.Extra.filter (selectedsymbols > 0 && withinsignview)
                        (andThen update (StartDragging))
                    |> Update.Extra.filter (howmanysymbolsunderposition == 1 && withinsignview)
                        (andThen update (SelectSymbol firstsymbolid))
                    |> Update.Extra.filter (howmanysymbolsunderposition == 0 && withinsignview)
                        (andThen update (UnSelectSymbols)
                            >> andThen update StartRectangleSelect
                        )

        MouseUp position ->
            let
                signviewposition =
                    (signViewPosition position model.viewposition)

                withinsignview =
                    (withinSignView signviewposition model.viewposition)

                symbolsunderposition =
                    (symbolsUnderPosition signviewposition model.sign.syms)
            in
                model
                    ! []
                    |> Update.Extra.filter (model.editormode == RectangleSelect)
                        (andThen update EndRectangleSelect)
                    |> Update.Extra.filter (model.editormode == Dragging)
                        (andThen update EndDragging)

        MouseMove position ->
            let
                signviewposition =
                    signViewPosition position model.viewposition

                withinsignview =
                    withinSignView signviewposition model.viewposition

                symbolsunderposition =
                    symbolsUnderPosition signviewposition model.sign.syms
            in
                { model | xy = signviewposition }
                    ! []
                    |> Update.Extra.filter (model.windowresized)
                        (andThen update UpdateSignViewPosition)
                    |> Update.Extra.filter (model.editormode == Dragging)
                        (andThen update DragSelected)

        DragSymbol code ->
            let
                symb1 =
                    getSymbolEditorCode code <| Dict.fromList [ ( "", { width = 20, height = 20 } ) ]

                symbol =
                    { symb1 | selected = True }

                sign1 =
                    unselectSymbols model.sign

                sign =
                    { sign1
                        | syms = List.append sign1.syms [ { symbol | x = model.xy.x, y = model.xy.y, id = model.uid + 1 } ]
                    }

                lastuid =
                    getlastuid <| sign
            in
                { model | uid = lastuid, editormode = Dragging, dragstart = model.xy, dragsign = sign } ! []



-- { model | editormode = Dragging, dragstart = model.xy, dragsign = model.sign } ! []


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
        , subDragSymbol DragSymbol
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
