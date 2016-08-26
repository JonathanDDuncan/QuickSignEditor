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


-- import SubSWEditors.State


init : ( Model, Cmd Msg )
init =
    ( Model "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468" signinit (Position 200 200) Nothing 0, Cmd.none )


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
update action ({ position, drag } as model) =
    case action of
        Change newfsw ->
            ( { model | fsw = newfsw, sign = signinit }, Cmd.none )

        RequestSign ->
            ( model, Ports.requestSign model.fsw )

        SetSign newsign ->
            let
                editorSign =
                    toEditorSign newsign model.uid

                lastuid =
                    getlastuid editorSign
            in
                ( { model | sign = editorSign, uid = lastuid }, Cmd.none )

        DragStart xy ->
            ( { model | position = position, drag = (Just (Drag xy xy)) }, Cmd.none )

        DragAt xy ->
            ( { model | position = position, drag = (Maybe.map (\{ start } -> Drag start xy) drag) }, Cmd.none )

        DragEnd _ ->
            ( { model | position = (SWEditor.Types.getPosition model), sign = Debug.log "UpdateSign" <| updateSignDrag model, drag = Nothing }, Cmd.none )

        Select id ->
            let
                toggleSelection sign symbol =
                    if sign.id == id then
                        { symbol | selected = not symbol.selected }
                    else
                        symbol
            in
                { model | sign = selectSymbol id model.sign, drag = Nothing }
                    ! []

getlastuid: EditorSign -> Int
getlastuid editorSign =
        case maximumBy .id editorSign.syms  of  
            Nothing -> 0
            Just sign -> sign.id 

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


updateSignDrag : Model -> EditorSign
updateSignDrag model =
    let
        offset =
            getOffset model

        sign =
            model.sign

        movedsyms =
            moveSymbols sign.syms offset
    in
        { sign | syms = movedsyms }


moveSymbols : List EditorSymbol -> Offset -> List EditorSymbol
moveSymbols symbols offset =
    List.map (moveSymbol offset) symbols


moveSymbol : Offset -> EditorSymbol -> EditorSymbol
moveSymbol offset symbol =
    if symbol.selected then
        { symbol | x = symbol.x + offset.offsetx, y = symbol.y + offset.offsety }
    else
        symbol


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, receiveSign SetSign ]


toEditorSign sign id =
    { signinit
        | width = sign.width
        , height = sign.height
        , text = sign.text
        , x = sign.x
        , y = sign.y
        , backfill = sign.backfill
        , syms = List.indexedMap (toEditorSymbol  id) sign.syms
        , laned = sign.laned
        , left = sign.left
    }


toEditorSymbol id index symbol =
    { symbolinit
        | x = symbol.x
        , y = symbol.y
        , width = symbol.width
        , height = symbol.height
        , fontsize = symbol.fontsize
        , nwcolor = symbol.nwcolor
        , pua = symbol.pua
        , code = symbol.code
        , key = symbol.key
        , nbcolor = symbol.nbcolor
        , selected = False
        , id = id + index + 1
    }



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
