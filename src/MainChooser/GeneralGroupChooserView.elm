module MainChooser.GeneralGroupChooserView exposing (generalgroupchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import Material.Tooltip as Tooltip exposing (attach, render, left)
import Material.Options as Options exposing (div, cs, when)


generalgroupchooser : List (List GeneralGroupChooserColumData) -> Html Msg
generalgroupchooser tabledata =
    table []
        (List.map row tabledata)


row : List GeneralGroupChooserColumData -> Html Msg
row rowdata =
    tr
        []
        (List.map column rowdata)


column : GeneralGroupChooserColumData -> Html Msg
column columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => (bkcolor columndata.row columndata.col) ]
        ]
        (List.map
            symbol
            columndata.symboldatalist
        )


symbol : GeneralGroupChooserSymbolData -> Html Msg
symbol symboldata =
    Html.div
        [ onClick (GroupSelected symboldata.chooseritem)
        , onMouseDown (DragSymbol symboldata.symbol.code)
        , onDoubleClick (ReplaceSymbol symboldata.symbol.code)
        ]
        [ Options.div
            [ Tooltip.attach Mdl [ symboldata.mdlid ] ]
            [ Html.map SignView
                (symbolaloneView symboldata.symbol 3)
            ]
        , Tooltip.render Mdl
            [ symboldata.mdlid ]
            symboldata.modelmdl
            [ Tooltip.left ]
            [ Html.div [ attribute "style" "float:left;" ] [ text symboldata.chooseritem.name ]
            ]
        ]
