module SWEditor.SymbolToolTip exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Choosers.Types exposing (..)
import Material exposing (..)
import Material.Tooltip as Tooltip exposing (..)
import Material.Options as Options exposing (div, cs, when)
import Choosers.HandPng exposing (..)
import SW.Types exposing (..)


symboltooltip :
    Material.Model
    -> Int
    -> String
    -> String
    -> Int
    -> Int
    -> HandFills
    -> List (Html Choosers.Types.Msg)
    -> List (Html Choosers.Types.Msg)
symboltooltip mdl mdlid name key rotation fill handfill content =
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


tooltipbody : List (Html Choosers.Types.Msg) -> String -> List (Html Choosers.Types.Msg)
tooltipbody handpngs name =
    List.append handpngs
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
