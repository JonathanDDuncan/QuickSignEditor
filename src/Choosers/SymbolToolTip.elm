module Choosers.SymbolToolTip exposing (handsymboltooltip, symboltooltip)

import Html exposing (Html, text, br)
import Html.Attributes exposing (attribute)
import Choosers.Types exposing (Msg(Mdl))
import Material
import Material.Tooltip as Tooltip
import Material.Options as Options
import Choosers.HandPng exposing (gethandpng, handpngspan)
import SW.Pua exposing (ishand)
import SW.HandFillsType exposing (HandFills(..))


handsymboltooltip :
    Material.Model
    -> Int
    -> String
    -> String
    -> Int
    -> HandFills
    -> List (Html Choosers.Types.Msg)
    -> List (Html Choosers.Types.Msg)
handsymboltooltip mdl mdlid name key rotation handfill content =
    let
        handpngs =
            if ishand key then
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


tooltipname : String -> List (Html msg)
tooltipname name =
    [ Html.div [ attribute "style" "width:100%;" ] [ text name ]
    ]


handpnglist : String -> Int -> HandFills -> List (Html c)
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
