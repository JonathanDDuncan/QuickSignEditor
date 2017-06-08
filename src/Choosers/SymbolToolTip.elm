module Choosers.SymbolToolTip exposing (handsymboltooltip, symboltooltip)

import Html exposing (Html, div, text, br)
import Html.Attributes exposing (..)
import Choosers.Types exposing (..)
import Material exposing (..)
import Material.Tooltip as Tooltip exposing (..)
import Material.Options as Options exposing (div, cs, when)
import Choosers.HandPng exposing (..)
import SW.Types exposing (..)


handsymboltooltip :
    Material.Model
    -> Int
    -> String
    -> String
    -> Int
    -> Int
    -> HandFills
    -> List (Html Choosers.Types.Msg)
    -> List (Html Choosers.Types.Msg)
handsymboltooltip mdl mdlid name key rotation fill handfill content =
    let
        ishand =
            iskey key "hand"

        handpngs =
            if ishand then
                handpnglist key rotation handfill
            else
                []
    in
        [ Options.div
            [ Tooltip.attach Mdl [ mdlid ] ]
            content
        , Tooltip.render Mdl
            [ mdlid ]
            mdl
            [ Tooltip.left ]
            (tooltipbody handpngs name)
        ]


symboltooltip :
    Material.Model
    -> Int
    -> String
    -> List (Html Choosers.Types.Msg)
    -> List (Html Choosers.Types.Msg)
symboltooltip mdl mdlid name content =
    [ Options.div
        [ Tooltip.attach Mdl [ mdlid ] ]
        content
    , Tooltip.render Mdl
        [ mdlid ]
        mdl
        [ Tooltip.left ]
        (tooltipname name)
    ]


tooltipbody : List (Html Choosers.Types.Msg) -> String -> List (Html Choosers.Types.Msg)
tooltipbody handpngs name =
    List.append handpngs (tooltipname name)


tooltipname name =
    [ Html.div [ attribute "style" "width:100%;" ] [ text name ]
    ]


handpnglist key rotation handfill =
    let
        handpngs1 =
            List.range 1 3
                |> List.map (\fill -> gethandpng key rotation fill handfill)

        handpngs2 =
            List.range 4 6
                |> List.map (\fill -> gethandpng key rotation fill handfill)

        pngs1 =
            List.map (\handpng -> handpngspan handpng "" "margin: auto;" "" "inline-block") handpngs1

        pngs2 =
            List.map (\handpng -> handpngspan handpng "" "margin: auto;" "" "inline-block") handpngs2
    in
        List.concat [ pngs1, [ br [] [] ], pngs2 ]
