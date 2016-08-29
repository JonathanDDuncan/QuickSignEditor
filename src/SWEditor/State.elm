module SWEditor.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import SWEditor.Types exposing (..)
import Ports as Ports exposing (..)
import Mouse exposing (Position)
import Debug
import SWEditor.Rectangle exposing (..)
import Update.Extra exposing (..)


-- import SubSWEditors.State


init : ( Model, Cmd Msg )
init =
    ( { fsw = "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"
      , sign = signinit
      , xy = (Position 0 0)
      , drag = Nothing
      , rectanglestart = Position 0 0
      , rectangleend = Position 0 0
      , rectangleselecting = False
      , windowresized = False
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
        Change newfsw ->
            ( { model | fsw = newfsw, sign = signinit }, Cmd.none )

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        SetSign newsign ->
            let 
                editorSign =
                    centerSign model (toEditorSign newsign model.uid)

                lastuid =
                    getlastuid editorSign
            in
                ( { model | sign = editorSign, uid = lastuid }, Cmd.none ) |> andThen update UpdateSignViewPosition

        RequestElementPosition elementid ->
            ( model, Ports.requestElementPosition elementid )

        ReceiveElementPosition namedposition ->
            { model | viewposition = Debug.log "View Position" namedposition } ! [] |> andThen update CenterSign

        UpdateSignViewPosition ->
            model ! [] |> andThen update (RequestElementPosition "signView")

        CenterSign ->
            ( { model | sign = centerSign model model.sign }, Cmd.none )

        SelectSignsInRectangle ->
            ( { model | rectangleend = Debug.log "DrawRectangleEnd xy" Position 0 0, rectangleselecting = False, sign = selectSymbolsIntersection (rectangleSelect model) model }, Cmd.none )

        SymbolMouseDown id ->
            let
                foundSymbol =
                    findSymbol id model.sign.syms

                isSelected =
                    case foundSymbol of
                        Nothing ->
                            False

                        Just val ->
                            val.selected
            in
                if isSelected then
                    ( { model
                        | drag =
                            (Just (Drag model.xy (Debug.log "DragStart xy" model.xy)))
                            -- , sign = selectSymbol (Debug.log "Select id" id) model.sign
                      }
                    , Cmd.none
                    )
                else
                    ( { model | sign = selectSymbol (Debug.log "Select id" id) model.sign }
                    , Cmd.none
                    )

        DragAt xy ->
            ( { model
                | xy = xy
                , drag = (Maybe.map (\{ start } -> Drag start (Debug.log "DragAt xy" xy)) drag)
              }
            , Cmd.none
            )

        DragEnd _ ->
            ( { model | xy = (SWEditor.Types.getPosition model), sign = updateSignDrag model, drag = (Debug.log "DragEnd" Nothing) }, Cmd.none )

        DrawRectangleStart xy ->
            let
                { offsetx, offsety } =
                    getOffset model
            in
                ( { model | rectanglestart = Debug.log "DrawRectangleStart xy" xy, rectangleselecting = True, sign = unselectSymbols model.sign }, Cmd.none )

        DrawRectangleAt xy ->
            ( { model | rectangleend = Debug.log "DrawRectangleAt xy" xy }, Cmd.none )

        DrawRectangleEnd _ ->
            model ! [] |> andThen update UpdateSignViewPosition

        MouseDown position ->
            let
                dbg =
                    Debug.log "MouseDown position" position

                signviewposition =
                    Debug.log "signviewposition" (signViewPosition position model)

                withinsignview =
                    Debug.log "signviewposition" (withinSignView signviewposition model)

                symbolsunderposition =
                    Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            in
                model ! []

        MouseUp position ->
            let
                dbg =
                    Debug.log "MouseUp position" position

                signviewposition =
                    Debug.log "signviewposition" (signViewPosition position model)

                withinsignview =
                    Debug.log "signviewposition" (withinSignView signviewposition model)

                symbolsunderposition =
                    Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            in
                model ! []

        MouseMove position ->
            let
                dbg =
                    Debug.log "MouseMove position" position

                signviewposition =
                    Debug.log "signviewposition" (signViewPosition position model)

                withinsignview =
                    Debug.log "signviewposition" (withinSignView signviewposition model)

                symbolsunderposition =
                    Debug.log "symbolsunderposition" (symbolsUnderPosition signviewposition model.sign)
            in
                model
                    ! []
                    |> filter (model.windowresized)
                        (andThen update UpdateSignViewPosition)


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
            Debug.log "desiredxcenter" desiredxcenter - Debug.log "currentxcenter" currentxcenter

        movey =
            Debug.log "desiredycenter" desiredycenter - Debug.log "currentycenter" currentycenter

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
        { sign | syms = List.map (selectIntersected rectangle model) sign.syms }


selectIntersected : Rect -> Model -> EditorSymbol -> EditorSymbol
selectIntersected rectangle model symbol =
    let
        symbolrect =
            getsymbolRectangle symbol

        selectRectangle =
            { rectangle
                | x =
                    rectangle.x
                        - model.viewposition.x
                , y =
                    rectangle.y
                        - model.viewposition.y
            }
    in
        if Debug.log "intersect" (intersect (Debug.log "Select Rect" selectRectangle) (Debug.log "Symbol Rect" symbolrect)) then
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


rectangleSelect : Model -> Rect
rectangleSelect model =
    let
        x1 =
            min model.rectanglestart.x model.rectangleend.x

        x2 =
            max model.rectanglestart.x model.rectangleend.x

        y1 =
            min model.rectanglestart.y model.rectangleend.y

        y2 =
            max model.rectanglestart.y model.rectangleend.y

        offset =
            getOffset model
    in
        { x = x1 + offset.offsetx
        , y = y1 + offset.offsety
        , width = x2 - x1
        , height = y2 - y1
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
        [ Mouse.downs MouseDown
        , Mouse.moves MouseMove
        , Mouse.ups MouseUp
        , receiveSign SetSign
        , receiveElementPosition ReceiveElementPosition
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
