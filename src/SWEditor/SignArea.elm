module SWEditor.SignArea exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
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
