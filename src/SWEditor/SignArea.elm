module SWEditor.SignArea exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)


deletesymbols : Model -> Model
deletesymbols model =
    let
        newsyms =
            List.filter (\syms -> not syms.selected) model.sign.syms

        newsign =
            updatesignsymbols model.sign newsyms
    in
        { model | sign = newsign }


updatesignsymbols : EditorSign -> List EditorSymbol -> EditorSign
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


removenewlydroppedsymbolifnotinview model =
    let
        undroppedsymbol =
            getundroppedsymbol model.editormode model.sign.syms

        iswithinview =
            issymbolwithinview model.viewposition undroppedsymbol
    in
        if not iswithinview then
            deletesymbols model
        else
            model


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
    -> Maybe EditorSymbol
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


movesymbol : EditorSymbol -> Direction -> Distance -> EditorSymbol
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


putsymbolswithinbounds : EditorSign -> { f | height : number, width : number' } -> EditorSign
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


maintainwithinbounds : EditorSymbol -> { b | height : number'', width : number''' } -> EditorSymbol
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
