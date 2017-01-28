module MainChooser.View exposing (root)

import Html exposing (..)
import String exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import Choosing.View exposing (..)
import Choosing.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import MainChooser.HandGroupChooserView exposing (..)
import MainChooser.GeneralGroupChooserView exposing (..)
import MainChooser.GeneralSymbolChooserView exposing (..)
import MainChooser.HandSymbolChooserView exposing (..)
import SW.Types exposing (iskey)
import SWEditor.EditorSymbol exposing (..)
import Exts.List exposing (..)


--import SubMainChooser.View exposing (root)


root : MainChooser.Types.Model -> Int -> Int -> Html MainChooser.Types.Msg
root model parentwidth parentheight =
    let
        halfheight =
            (Basics.truncate ((Basics.toFloat parentheight) / Basics.toFloat 2))

        halfwidth =
            (Basics.truncate ((Basics.toFloat parentwidth) / Basics.toFloat 2))
    in
        div []
            [ div
                [ style [ "height" => px (halfheight - 30) ] ]
                (List.map displayChoosing model.choosings)
            , div
                [ class "generalsymbolchooser"
                , style
                    [ "height" => px halfheight
                    , "display" => "inline-block"
                    , "margin-top" => "5px"
                    , "float" => "left"
                    ]
                ]
                [ symbolchooser model halfwidth halfheight
                ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => "50%"
                    , "left" => "0px"
                    , "top" => "0px"
                    , "margin-left" => "50%"
                    , "height" => px parentheight
                    , "margin-top" => "5px"
                    , "overflow-y" => "scroll"
                    , "overflow-x" => "scroll"
                    ]
                ]
                [ choosesubgroupchooser model
                ]
            ]


symbolchooser : MainChooser.Types.Model -> Int -> Int -> Html MainChooser.Types.Msg
symbolchooser model halfwidth halfheight =
    if iskey model.groupselected.symbolkey "hand" then
        handsymbolchooser model.handsymbol model.groupselected model.symbolsizes halfwidth halfheight
    else
        generalsymbolchooser model.groupselected model.selectedcolumn model.symbolsizes halfwidth halfheight


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div
        [ onClick (Clicked choosing.value)
        , onMouseDown (DragSymbol (firstsymbol choosing).code)
        , onDoubleClick
            (ReplaceSymbol (firstsymbol choosing).code)
        ]
        [ Html.map Choosing (Choosing.View.root choosing) ]


firstsymbol : { b | valuestoAdd : List EditorSymbol } -> EditorSymbol
firstsymbol choosing =
    choosing.valuestoAdd |> List.head |> Maybe.withDefault SWEditor.EditorSymbol.symbolinit


choosesubgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
choosesubgroupchooser model =
    let
        basesymbol =
            String.slice 0 4 model.clicked
    in
        case basesymbol of
            "S14c" ->
                handgroupchooser2 model

            _ ->
                generalgroupchooser2 model basesymbol



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]



--Put this in state later


generalgroupchooser2 : MainChooser.Types.Model -> String -> Html MainChooser.Types.Msg
generalgroupchooser2 model basesymbol =
    let
        choosings =
            getchoosings basesymbol model.allgroupchoosings

        generalgroupchooserdata =
            creategeneralgroupchooserdata model choosings
    in
        generalgroupchooser generalgroupchooserdata


creategeneralgroupchooserdata :
    MainChooser.Types.Model
    -> List ChooserItem
    -> List (List GeneralGroupChooserColumData)
creategeneralgroupchooserdata model choosings =
    let
        rowvalues =
            List.sort <| unique <| List.map (\item -> item.row) choosings

        tabledata =
            creategeneralgroupchoosertabledata model choosings rowvalues
    in
        tabledata


creategeneralgroupchoosertabledata : MainChooser.Types.Model -> List ChooserItem -> List Int -> List (List GeneralGroupChooserColumData)
creategeneralgroupchoosertabledata model choosings rowvalues =
    List.map
        (\row ->
            creategeneralgroupchooserrowdata model row choosings
        )
        rowvalues


creategeneralgroupchooserrowdata : MainChooser.Types.Model -> Int -> List ChooserItem -> List GeneralGroupChooserColumData
creategeneralgroupchooserrowdata model row choosings =
    let
        colvalues =
            List.sort <| unique <| List.map (\item -> item.col) choosings

        rowdata =
            (List.map (\col -> creategeneralgroupchoosercolumndata model row col choosings) colvalues)
    in
        rowdata


creategeneralgroupchoosercolumndata : MainChooser.Types.Model -> Int -> Int -> List ChooserItem -> GeneralGroupChooserColumData
creategeneralgroupchoosercolumndata model row col choosings =
    let
        choosingsforcolumn =
            List.filter (\item -> item.col == col) choosings

        symboldatalist =
            (choosingsforcolumn
                |> List.map
                    (\chooseritem ->
                        creategeneralgroupchoosersymboldata model chooseritem
                    )
            )
    in
        { symboldatalist = symboldatalist, row = row, col = col }


creategeneralgroupchoosersymboldata : MainChooser.Types.Model -> ChooserItem -> GeneralGroupChooserSymbolData
creategeneralgroupchoosersymboldata model chooseritem =
    let
        symbol =
            getSymbolEditorBaseFillRotation chooseritem.base 1 1 model.symbolsizes

        mdlid =
            symbol.code + 1000

        modelmdl =
            model.mdl
    in
        { modelmdl = modelmdl, chooseritem = chooseritem, symbol = symbol, mdlid = mdlid }



--move this hand group chooser to state


handgroupchooser2 : MainChooser.Types.Model -> Html MainChooser.Types.Msg
handgroupchooser2 model =
    let
        tabledata =
            createtabledata model
    in
        handgroupchooser tabledata


createtabledata : MainChooser.Types.Model -> List (List (List HandGroupChooserViewColumnData))
createtabledata model =
    (List.map
        (\data ->
            let
                tabledata2 =
                    createrowdata model data
            in
                tabledata2
        )
        model.handgroupchooseritems
    )


createrowdata : MainChooser.Types.Model -> List (List HandGroupChooserSubList) -> List (List HandGroupChooserViewColumnData)
createrowdata model tabledata =
    let
        filtered =
            List.filter
                (\columndata ->
                    List.length columndata > 0
                )
                tabledata

        rowdata =
            List.map
                (\rowdata1 ->
                    createcolumndata model rowdata1
                )
                filtered
    in
        rowdata


createcolumndata : MainChooser.Types.Model -> List HandGroupChooserSubList -> List HandGroupChooserViewColumnData
createcolumndata model rowdata =
    (List.map
        (\coldata ->
            let
                symboldatalist =
                    createsymboldatalist model coldata
            in
                { symboldatalist = symboldatalist, backgroundcolor = coldata.backgroundcolor }
        )
        rowdata
    )


createsymboldatalist : MainChooser.Types.Model -> HandGroupChooserSubList -> List HandGroupChooserViewSymbolData
createsymboldatalist model columndata =
    List.map
        (\displayhanditem ->
            { modelmdl = model.mdl
            , symbol = displayhanditem.symbol
            , chooseritem = displayhanditem.chooseritem
            , mdlid = displayhanditem.mdlid
            }
        )
        columndata.displayhanditems
