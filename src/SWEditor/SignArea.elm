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
                s =
                    Debug.log "symbol" <| symbol

                symbolbounds =
                    Debug.log "symbolbounds" <|
                        { left = symbol.x
                        , right = symbol.x + symbol.width
                        , top = symbol.y
                        , bottom = symbol.y + symbol.height
                        }

                viewbounds =
                    Debug.log "viewbounds" <|
                        { left = 0
                        , right = 0 + viewposition.width
                        , top = 0
                        , bottom = 0 + viewposition.height
                        }

                withinright =
                    Debug.log "withinright" <|
                        symbolbounds.right
                            <= viewbounds.right

                withinleft =
                    Debug.log "withinleft" <|
                        symbolbounds.left
                            >= viewbounds.left

                withinbottom =
                    Debug.log "withinbottom" <|
                        symbolbounds.bottom
                            <= viewbounds.bottom

                withintop =
                    Debug.log "withintop" <|
                        symbolbounds.top
                            >= viewbounds.top

                iswithin =
                    Debug.log "iswithin" <|
                        withinleft
                            && withinright
                            && withintop
                            && withinbottom
            in
                iswithin

        Nothing ->
            True
