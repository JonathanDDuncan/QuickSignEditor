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
    ( { fsw = "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"
      , sign = signinit
      , xy = (Position 200 200)
      , drag = Nothing
      , rectanglestart = Position 10000000000000000 100000000000000000
      , rectangleend = Position 100000000000000 10000000000000
      , rectangleselecting = False
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
                    toEditorSign newsign model.uid

                lastuid =
                    getlastuid editorSign
            in
                ( { model | sign = editorSign, uid = lastuid }, Cmd.none )

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
            let
                { offsetx, offsety } =
                    getOffset' model xy
            in
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
                -- if offsetx == 0 && offsety == 0 then
                --     (   model  , Cmd.none )
                -- else
                ( { model | rectanglestart = Debug.log "DrawRectangleStart xy" xy, rectangleselecting = True, sign = unselectSymbols model.sign }, Cmd.none )

        DrawRectangleAt xy ->
            ( { model | rectangleend = Debug.log "DrawRectangleAt xy" xy }, Cmd.none )

        DrawRectangleEnd _ ->
            ( { model | rectangleend = Debug.log "DrawRectangleEnd xy" Position 0 0, rectangleselecting = False }, Cmd.none )


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
            moveSymbols sign.syms offset
    in
        { sign | syms = movedsyms }


unselectSymbols : EditorSign -> EditorSign
unselectSymbols sign =
 { sign | syms =   List.map unselectSymbol sign.syms }
  


unselectSymbol : EditorSymbol -> EditorSymbol
unselectSymbol symbol =
    { symbol | selected = False }


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
    Sub.batch
        [ Mouse.moves DragAt
        , Mouse.ups DragEnd
          , Mouse.moves DrawRectangleAt
          , Mouse.ups DrawRectangleEnd
        , receiveSign SetSign
        ]



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
