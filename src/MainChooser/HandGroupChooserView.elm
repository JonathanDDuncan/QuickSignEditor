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
    [ if (List.length rowdata.datawithoutthumbs) > 0 then
        tr
            []
            (List.map (\coldata -> column model coldata) rowdata.datawithoutthumbs)
      else
        text ""
    , if (List.length rowdata.datawiththumbs) > 0 then
        tr
            []
            (List.map (\coldata -> column model coldata) rowdata.datawiththumbs)
      else
        text ""
    ]


column model columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => columndata.backgroundcolor ]
        ]
        (List.map (displayhandChoosing model) columndata.displayhanditems)


displayhandChoosing model displayhanditem =
    Html.div
        [ Html.Events.onClick (GroupSelected displayhanditem.chooseritem)
        ]
        [ Options.div
            [ Tooltip.attach Mdl [ displayhanditem.mdlid ] ]
            [ App.map SignView
                (symbolaloneView displayhanditem.symbol 5)
            ]
        , Tooltip.render Mdl
            [ displayhanditem.mdlid ]
            model.mdl
            [ Tooltip.left ]
            [ span [ class (handpngcss displayhanditem.chooseritem.symbolkey), attribute "style" "display:inline-block ;margin: auto;" ] []
            , Html.div [ attribute "style" "width:100%;" ] [ text displayhanditem.chooseritem.name ]
            ]
        ]


handpngcss : String -> String
handpngcss key =
    String.toLower "hands-" ++ String.slice 1 4 key ++ "10"
