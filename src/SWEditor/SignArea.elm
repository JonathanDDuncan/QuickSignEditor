module SWEditor.SignArea exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
import SW.Types exposing (..)
import SWEditor.Select exposing (..)
import SWEditor.EditorSymbol exposing (..)


deletesymbols : Model -> Model
deletesymbols model =
    let
        newsyms =
            List.filter (\syms -> not syms.selected) model.sign.syms

        newsign =
            updatesignsymbols model.sign newsyms
    in
        { model | sign = newsign }


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
            getlastuid newsymbols

        newsyms =
            List.concat [ unselectedsyms, (unselectSymbols selectedsyms), newsymbols ]

        newsign =
            updatesignsymbols model.sign newsyms
    in
        { model | sign = newsign, uid = lastuid }


updatesignsymbols : Sign -> List Symbol -> Sign
updatesignsymbols sign newsymbols =
    let
        bounds =
            getSignBounding newsymbols

        newsign =
            { sign
                | syms = newsymbols
                , width = bounds.width
                , height = bounds.height
                , x = bounds.x
                , y = bounds.y
            }
    in
        newsign


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


issymbolwithinview :
    NamedPosition
    -> Maybe Symbol
    -> Bool
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

                iswithin =
                    withinleft
                        && withinright
                        && withintop
                        && withinbottom
            in
                iswithin

        Nothing ->
            True


movesymbols : Model -> Direction -> Distance -> Model
movesymbols model direction distance =
    let
        newsymbols =
            List.map
                (\symbol ->
                    if symbol.selected then
                        maintainwithinbounds (movesymbol symbol direction distance) model.viewposition
                    else
                        symbol
                )
                model.sign.syms

        newsign =
            updatesignsymbols model.sign newsymbols
    in
        { model | sign = newsign }


movesymbol : Symbol -> Direction -> Distance -> Symbol
movesymbol symbol direction distance =
    let
        newsymbol =
            case direction of
                Up ->
                    { symbol | y = symbol.y - distance }

                Down ->
                    { symbol | y = symbol.y + distance }

                Right ->
                    { symbol | x = symbol.x + distance }

                Left ->
                    { symbol | x = symbol.x - distance }
    in
        newsymbol


putsymbolswithinbounds : Sign -> { f | height : number, width : number_ } -> Sign
putsymbolswithinbounds sign bounds =
    let
        movedsyms =
            List.map (\sym -> maintainwithinbounds sym <| bounds) sign.syms

        signbound =
            getSignBounding movedsyms
    in
        { sign
            | syms = movedsyms
            , width = signbound.width
            , height = signbound.height
            , x = signbound.x
            , y = signbound.y
        }


maintainwithinbounds : Symbol -> { b | height : number__, width : number___ } -> Symbol
maintainwithinbounds sym bounds =
    let
        left =
            0

        right =
            0 + bounds.width - 20

        top =
            5

        bottom =
            0 + bounds.height - 10

        newx =
            if (sym.x < left) then
                left
            else if sym.x + sym.width > right then
                right - sym.width
            else
                sym.x

        newy =
            if (sym.y < top) then
                top
            else if sym.y + sym.height > bottom then
                bottom - sym.height
            else
                sym.y
    in
        { sym | x = newx, y = newy }
