module SWEditor.SignArea
    exposing
        ( deletesymbols
        , getundroppedsymbol
        , issymbolwithinview
        , putsymbolswithinbounds
        , duplicatesymbols
        , changesizesymbols
        , movesymbols
        )

import SWEditor.Types exposing (Model, Msg, EditorMode(..), Direction(..), Distance)
import SW.Types exposing (NamedPosition)
import SW.Sign exposing (Sign)
import SW.Symbol exposing (Symbol)
import SWEditor.Select exposing (unselectSymbols)
import SW.Identifier exposing (updateIds, lastid)
import SW.Rectangle exposing (getBounding)


deletesymbols : Model -> Model
deletesymbols model =
    { model
        | sign =
            List.filter (\syms -> not syms.selected) model.sign.syms
                |> updatesignsymbols model.sign
    }


duplicatesymbols : Model -> Model
duplicatesymbols model =
    let
        selectedsyms =
            List.filter (\sym -> sym.selected) model.sign.syms

        unselectedsyms =
            List.filter (\sym -> not sym.selected) model.sign.syms

        newsymbols =
            List.map (\sym -> { sym | selected = True, x = sym.x + 5, y = sym.y + 5 }) selectedsyms
                |> updateIds model.uid

        lastuid =
            lastid newsymbols

        newsyms =
            List.concat [ unselectedsyms, unselectSymbols selectedsyms, newsymbols ]

        newsign =
            updatesignsymbols model.sign newsyms
    in
        { model | sign = newsign, uid = lastuid }


changesizesymbols : Model -> Float -> Model
changesizesymbols model sizediff =
    { model
        | sign =
            List.map
                (\sym ->
                    if sym.selected then
                        changesizesymbol sym sizediff
                    else
                        sym
                )
                model.sign.syms
                |> updatesignsymbols model.sign
    }


changesizesymbol : { a | size : Float } -> Float -> { a | size : Float }
changesizesymbol symbol sizediff =
    { symbol | size = symbol.size + sizediff }


updatesignsymbols : Sign -> List Symbol -> Sign
updatesignsymbols sign newsymbols =
    let
        bounds =
            getBounding newsymbols
    in
        { sign
            | syms = newsymbols
            , width = bounds.width
            , height = bounds.height
            , x = bounds.x
            , y = bounds.y
        }


getundroppedsymbol :
    EditorMode
    -> List { a | selected : Bool }
    -> Maybe { a | selected : Bool }
getundroppedsymbol editormode symbols =
    if editormode == AddingSymbol then
        symbols
            |> List.filter (\sym -> sym.selected)
            |> List.head
    else
        Nothing


issymbolwithinview : NamedPosition -> Maybe Symbol -> Bool
issymbolwithinview viewposition undroppedsymbol =
    case undroppedsymbol of
        Just symbol ->
            let
                symbolbounds =
                    { left = symbol.x
                    , right = symbol.x + symbol.width
                    , top = symbol.y
                    , bottom = symbol.y + symbol.height
                    }

                viewbounds =
                    { left = 0
                    , right = 0 + viewposition.width
                    , top = 0
                    , bottom = 0 + viewposition.height
                    }

                withinright =
                    symbolbounds.right
                        <= viewbounds.right

                withinleft =
                    symbolbounds.left
                        >= viewbounds.left

                withinbottom =
                    symbolbounds.bottom
                        <= viewbounds.bottom

                withintop =
                    symbolbounds.top
                        >= viewbounds.top
            in
                withinleft
                    && withinright
                    && withintop
                    && withinbottom

        Nothing ->
            True


movesymbols : Model -> Direction -> Distance -> Model
movesymbols model direction distance =
    { model
        | sign =
            List.map
                (\symbol ->
                    if symbol.selected then
                        maintainwithinbounds (movesymbol symbol direction distance) model.viewposition
                    else
                        symbol
                )
                model.sign.syms
                |> updatesignsymbols model.sign
    }


movesymbol : Symbol -> Direction -> Distance -> Symbol
movesymbol symbol direction distance =
    case direction of
        Up ->
            { symbol | y = symbol.y - distance }

        Down ->
            { symbol | y = symbol.y + distance }

        Right ->
            { symbol | x = symbol.x + distance }

        Left ->
            { symbol | x = symbol.x - distance }


putsymbolswithinbounds : Sign -> { f | height : Int, width : Int } -> Sign
putsymbolswithinbounds sign bounds =
    let
        movedsyms =
            List.map (\sym -> maintainwithinbounds sym <| bounds) sign.syms

        signbound =
            getBounding movedsyms
    in
        { sign
            | syms = movedsyms
            , width = signbound.width
            , height = signbound.height
            , x = signbound.x
            , y = signbound.y
        }


maintainwithinbounds : Symbol -> { b | height : Int, width : Int } -> Symbol
maintainwithinbounds sym bounds =
    let
        left =
            0

        right =
            0 + bounds.width - 20

        top =
            0

        bottom =
            0 + bounds.height - 10

        newx =
            if sym.x < left then
                left
            else if sym.x + sym.width > right then
                right - sym.width
            else
                sym.x

        newy =
            if sym.y < top then
                top
            else if sym.y + sym.height > bottom then
                bottom - sym.height
            else
                sym.y
    in
        { sym | x = newx, y = newy }
