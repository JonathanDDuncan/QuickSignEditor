module MainChooser.HandGroupChooserView exposing (handgroupchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import SWEditor.SymbolToolTip exposing (..)


handgroupchooser : List (List (List HandGroupChooserViewColumnData)) -> Html Msg
handgroupchooser tabledata =
    Html.div []
        [ Html.div []
            [ button [ Html.Events.onClick (FilterHandGroup 1) ] [ text "common" ]
            , button [ Html.Events.onClick (FilterHandGroup 2) ] [ text "not common" ]
            , button [ Html.Events.onClick (FilterHandGroup 3) ] [ text "all" ]
            ]
        , table []
            (List.concatMap (List.map row) tabledata)
        ]


row : List HandGroupChooserViewColumnData -> Html Msg
row rowdata =
    tr
        []
        (List.map column rowdata)


column : HandGroupChooserViewColumnData -> Html Msg
column columndata =
    td
        [ class "chosercolumn"
        , style
            [ "background-color" => columndata.backgroundcolor ]
        ]
        (List.map symbol columndata.symboldatalist)


symbol : HandGroupChooserViewSymbolData -> Html Msg
symbol symboldata =
    Html.div
        [ Html.Events.onClick (GroupSelected symboldata.chooseritem)
        , onMouseDown (DragSymbol symboldata.symbol.code)
        , onDoubleClick (ReplaceSymbol symboldata.symbol.code)
        ]
        (symboltooltip
            symboldata.modelmdl
            symboldata.mdlid
            symboldata.chooseritem.name
            symboldata.chooseritem.symbolkey
            1
            2
            RightThumbEdge
            [ Html.map SignView
                (symbolaloneView symboldata.symbol 5)
            ]
        )
