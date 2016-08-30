module SWEditor.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import SWEditor.Types exposing (..)
import Ports as Ports exposing (..)
import Debug
import SWEditor.Rectangle exposing (..)
import Update.Extra exposing (..)
import Touch.TouchEvents as Events exposing (..)


-- import SubSWEditors.State


init : ( Model, Cmd Msg )
init =
    ( { fsw = "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"
      , sign = signinit
      , xy = (Position 0 0)
      , drag = Nothing
      , dragstart = Position 0 0
      , dragend = Position 0 0
      , dragsign = signinit
      , rectanglestart = Position 0 0
      , rectangleend = Position 0 0
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
update action ({ drag } as model) =
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
                { model | xy = signviewposition, rectangleend = signviewposition, dragend = signviewposition }
                    ! []
                    |> filter (model.windowresized)
                        (andThen update UpdateSignViewPosition)
                    |> filter (model.editormode == Dragging)
                        (andThen update DragSelected)


rectangleselect : Model -> EditorSign
rectangleselect model =
    selectSymbolsIntersection (rectangleSelect model) model


dragsign : Model -> EditorSign
dragsign model =
    let
        dragoffset =
            Offset (model.dragend.x - model.dragstart.x) (model.dragend.y - model.dragstart.y)

        sign =
            model.dragsign

        symbols =
            dragSymbols dragoffset sign.syms
    in
        { sign | syms = symbols }


dragSymbols : Offset -> List EditorSymbol -> List EditorSymbol
dragSymbols offset symbols =
    List.map
        (\symbol ->
            if symbol.selected then
                { symbol | x = symbol.x + offset.offsetx, y = symbol.y + offset.offsety }
            else
                symbol
        )
        symbols


selectSymbolId : Int -> Model -> EditorSign
selectSymbolId id model =
    let
        sign =
            model.sign

        symbols =
            selectId id sign.syms
    in
        { sign | syms = symbols }


selectsymbol : Maybe EditorSymbol -> List EditorSymbol -> List EditorSymbol
selectsymbol symbol symbols =
    let
        symbolid =
            symbolId symbol
    in
        selectId symbolid symbols


symbolId : Maybe EditorSymbol -> Int
symbolId symbol =
    case symbol of
        Nothing ->
            0

        Just symb ->
            symb.id


countselectedsymbols : List EditorSymbol -> Int
countselectedsymbols symbols =
    List.length
        (List.filter
            (\symbol ->
                symbol.selected
            )
            symbols
        )


selectId : Int -> List EditorSymbol -> List EditorSymbol
selectId id symbols =
    List.map
        (\symbol ->
            if symbol.id == id then
                { symbol | selected = True }
            else
                symbol
        )
        symbols


signViewPosition : Position -> Model -> Position
signViewPosition position model =
    { x = position.x - model.viewposition.x, y = position.y - model.viewposition.y }


withinSignView : Position -> Model -> Bool
withinSignView signviewposition model =
    signviewposition.x >= 0 && signviewposition.y >= 0 && signviewposition.x <= model.viewposition.width && signviewposition.y <= model.viewposition.height


symbolsUnderPosition : Position -> EditorSign -> List EditorSymbol
symbolsUnderPosition signviewposition sign =
    let
        seachrectangle =
            { x = signviewposition.x, y = signviewposition.y, width = 1, height = 1 }
    in
        List.filter (\symbol -> (intersect seachrectangle (getsymbolRectangle symbol))) sign.syms


centerSign : Model -> EditorSign -> EditorSign
centerSign model sign =
    let
        width =
            model.viewposition.width

        height =
            model.viewposition.height

        bounding =
            getSignBounding sign

        currentxcenter =
            bounding.x + bounding.width // 2

        currentycenter =
            bounding.y + bounding.height // 2

        desiredxcenter =
            width // 2

        desiredycenter =
            height // 2

        movex =
            desiredxcenter - currentxcenter

        movey =
            desiredycenter - currentycenter

        newsignx =
            model.sign.x + movex

        newsigny =
            model.sign.y + movey
    in
        { sign | x = newsignx, y = newsigny, syms = moveSymbols movex movey sign.syms }


getSignBounding : EditorSign -> Rect
getSignBounding sign =
    let
        x1 =
            List.foldr (\s -> min s.x) 10000 sign.syms

        y1 =
            List.foldr (\s -> min s.y) 10000 sign.syms

        x2 =
            List.foldr (\s -> max (s.x + s.width)) 0 sign.syms

        y2 =
            List.foldr (\s -> max (s.y + s.height)) 0 sign.syms
    in
        { x = x1, y = y1, width = x2 - x1, height = y2 - y1 }


moveSymbols : Int -> Int -> List EditorSymbol -> List EditorSymbol
moveSymbols movex movey symbols =
    List.map (moveSymbol movex movey) symbols


moveSymbol : Int -> Int -> EditorSymbol -> EditorSymbol
moveSymbol movex movey symbol =
    { symbol | x = symbol.x + movex, y = symbol.y + movey }


selectSymbolsIntersection : Rect -> Model -> EditorSign
selectSymbolsIntersection rectangle model =
    let
        sign =
            model.sign
    in
        { sign | syms = List.map (selectIntersected rectangle) sign.syms }


selectIntersected : Rect -> EditorSymbol -> EditorSymbol
selectIntersected rectangle symbol =
    let
        symbolrect =
            getsymbolRectangle symbol

        selectRectangle =
            { rectangle
                | x =
                    rectangle.x
                , y =
                    rectangle.y
            }
    in
        if intersect selectRectangle symbolrect then
            { symbol | selected = True }
        else
            symbol


intersect : Rect -> Rect -> Bool
intersect rect1 rect2 =
    let
        rect1x2 =
            rect1.x + rect1.width

        rect1y2 =
            rect1.y + rect1.height

        rect2x2 =
            rect2.x + rect2.width

        rect2y2 =
            rect2.y + rect2.height
    in
        rect1.x < rect2x2 && rect1x2 > rect2.x && rect1.y < rect2y2 && rect1y2 > rect2.y


getsymbolRectangle : EditorSymbol -> Rect
getsymbolRectangle symbol =
    { x = symbol.x
    , y = symbol.y
    , width = symbol.width
    , height = symbol.height
    }


getlastuid : EditorSign -> Int
getlastuid editorSign =
    case maximumBy .id editorSign.syms of
        Nothing ->
            0

        Just sign ->
            sign.id


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f ls =
    let
        maxBy f x y =
            if (f x) > (f y) then
                x
            else
                y
    in
        case ls of
            l' :: ls' ->
                Just <| List.foldl (maxBy f) l' ls'

            _ ->
                Nothing


selectSymbol : Int -> EditorSign -> EditorSign
selectSymbol id sign =
    { sign | syms = List.map (toggleSymbolSelection id) sign.syms }


toggleSymbolSelection : Int -> EditorSymbol -> EditorSymbol
toggleSymbolSelection id symbol =
    if symbol.id == id then
        { symbol | selected = not symbol.selected }
    else
        symbol


findSymbol : Int -> List EditorSymbol -> Maybe EditorSymbol
findSymbol id symbols =
    List.head (List.filter (\s -> s.id == id) symbols)


updateSignDrag : Model -> EditorSign
updateSignDrag model =
    let
        offset =
            getOffset model

        sign =
            model.sign

        movedsyms =
            moveSymbolsOffset sign.syms offset
    in
        { sign | syms = movedsyms }


unselectSymbols : EditorSign -> EditorSign
unselectSymbols sign =
    { sign | syms = List.map unselectSymbol sign.syms }


unselectSymbol : EditorSymbol -> EditorSymbol
unselectSymbol symbol =
    { symbol | selected = False }


moveSymbolsOffset : List EditorSymbol -> Offset -> List EditorSymbol
moveSymbolsOffset symbols offset =
    List.map (moveSymbolOffset offset) symbols


moveSymbolOffset : Offset -> EditorSymbol -> EditorSymbol
moveSymbolOffset offset symbol =
    if symbol.selected then
        { symbol | x = symbol.x + offset.offsetx, y = symbol.y + offset.offsety }
    else
        symbol


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
