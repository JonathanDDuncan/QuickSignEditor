module SWEditor.SymbolToolTip exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import MainChooser.Types exposing (..)
import Material exposing (..)
import Material.Tooltip as Tooltip exposing (..)
import Material.Options as Options exposing (div, cs, when)
import MainChooser.HandPng exposing (..)
import SW.Types exposing (..)


symboltooltip :
    Material.Model
    -> Int
    -> String
    -> String
    -> Int
    -> Int
    -> HandFills
    -> List (Html MainChooser.Types.Msg)
    -> List (Html MainChooser.Types.Msg)
symboltooltip mdl mdlid name key rotation fill handfill content =
    let
        ishand =
            iskey key "hand"

        handpng =
            gethandpng key rotation fill handfill
    in
        [ Options.div
            [ Tooltip.attach Mdl [ mdlid ] ]
            content
        , Tooltip.render Mdl
            [ mdlid ]
            mdl
            [ Tooltip.left ]
            [ if ishand then
                handpngspan handpng "margin: auto;" ""
              else
                text ""
            , Html.div [ attribute "style" "width:100%;" ] [ text name ]
            ]
        ]
