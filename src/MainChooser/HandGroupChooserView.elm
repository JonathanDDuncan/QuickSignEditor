module MainChooser.HandGroupChooserView exposing (handgroupchooser)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import String exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import Material.Tooltip as Tooltip exposing (..)
import Material.Options as Options exposing (div, cs, when)
import MainChooser.HandPng exposing (..)
import SW.Types exposing (..)


handgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
handgroupchooser model =
    Html.div []
        [ Html.div []
            [ button [ Html.Events.onClick (FilterHandGroup 1) ] [ text "common" ]
            , button [ Html.Events.onClick (FilterHandGroup 2) ] [ text "not common" ]
            , button [ Html.Events.onClick (FilterHandGroup 3) ] [ text "all" ]
            ]
        , table []
            (List.concatMap (\data -> rowchooser model data) model.handgroupchooseritems)
        ]


rowchooser model rowdata =
    List.map
        (\l ->
            if (List.length l) > 0 then
                tr
                    []
                    (List.map (\coldata -> column model coldata) l)
            else
                text ""
        )
        rowdata


column model columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => columndata.backgroundcolor ]
        ]
        (List.map (displayhandChoosing model) columndata.displayhanditems)


displayhandChoosing model displayhanditem =
    let
        content =
            [ App.map SignView
                (symbolaloneView displayhanditem.symbol 5)
            ]
    in
        Html.div
            [ Html.Events.onClick (GroupSelected displayhanditem.chooseritem)
            ]
            (symboltooltip
                model.mdl
                displayhanditem.mdlid
                displayhanditem.chooseritem.name
                displayhanditem.chooseritem.symbolkey
                1
                2
                RightThumbEdge
                content
            )


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
