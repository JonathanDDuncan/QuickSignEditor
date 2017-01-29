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
import MainChooser.GeneralGroupChooser exposing (..)
import MainChooser.GeneralSymbolChooserView exposing (..)
import MainChooser.HandSymbolChooserView exposing (..)
import SW.Types exposing (iskey)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)


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
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser2 model.groupselected model.symbolsizes model.selectedcolumn halfwidth halfheight
    in
        if iskey model.groupselected.symbolkey "hand" then
            handsymbolchooser model.handsymbol model.groupselected model.symbolsizes halfwidth halfheight
        else
            generalsymbolchooser model.groupselected halfwidth halfheight generalsymbolchooserdata


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

        generalgroupchooserdata =
            creategeneralgroupchooserdata model basesymbol
    in
        case basesymbol of
            "S14c" ->
                handgroupchooser2 model

            _ ->
                generalgroupchooser generalgroupchooserdata



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]



--Put this in state later
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



---


getgeneralsymbolchooser2 :
    { a | base : Base, validfills : String, validrotations : String }
    -> Dict String Size
    -> Int
    -> b
    -> c
    -> { generalsymbolrowdata : List { fill : Int, symbol : EditorSymbol }
       , symbolcolumnsdata :
            List
                { generalsymbolonecolumndata :
                    { show1 : Bool
                    , show2 : Bool
                    , symbol1 : EditorSymbol
                    , symbol2 : EditorSymbol
                    }
                }
       }
getgeneralsymbolchooser2 choosing symbolsizes selectedcolumn width height =
    let
        validfills =
            choosing.validfills

        validrotations =
            choosing.validrotations

        base =
            choosing.base

        vf =
            getvalidfills validfills

        vr =
            getvalidrotations validrotations

        column =
            if isValidRotation selectedcolumn vf then
                selectedcolumn
            else
                1

        generalsymbolrowdata =
            getsymbolfill base 1 vf symbolsizes

        symbolcolumnsdata =
            getsymbolcolumnsdata base column vr symbolsizes
    in
        { generalsymbolrowdata = generalsymbolrowdata, symbolcolumnsdata = symbolcolumnsdata }


getsymbolcolumnsdata :
    Base
    -> Fill
    -> List Rotation
    -> Dict String Size
    -> List
        { generalsymbolonecolumndata :
            { show1 : Bool
            , show2 : Bool
            , symbol1 : EditorSymbol
            , symbol2 : EditorSymbol
            }
        }
getsymbolcolumnsdata base column vr symbolsizes =
    List.map
        (\rotation ->
            getrowdata base column rotation vr symbolsizes
        )
        (List.range 1 8)


getrowdata :
    Base
    -> Fill
    -> Int
    -> List Rotation
    -> Dict String Size
    -> { generalsymbolonecolumndata :
            { show1 : Bool
            , show2 : Bool
            , symbol1 : EditorSymbol
            , symbol2 : EditorSymbol
            }
       }
getrowdata base column rotation vr symbolsizes =
    let
        generalsymbolonecolumndata =
            getgeneralsymbolonecolumndata base column rotation vr symbolsizes
    in
        { generalsymbolonecolumndata = generalsymbolonecolumndata }


getgeneralsymbolonecolumndata :
    Base
    -> Fill
    -> Int
    -> List Rotation
    -> Dict String Size
    -> { show1 : Bool
       , symbol1 : EditorSymbol
       , symbol2 : EditorSymbol
       , show2 : Bool
       }
getgeneralsymbolonecolumndata base fill rotation validrotations symbolsizes =
    let
        rotation1 =
            rotation

        rotation2 =
            rotation + 8

        showrotation1 =
            isValidRotation rotation1 validrotations

        showrotation2 =
            isValidRotation rotation2 validrotations

        symbol1 =
            getSymbolEditorBaseFillRotation base fill rotation1 symbolsizes

        symbol2 =
            getSymbolEditorBaseFillRotation base fill rotation2 symbolsizes
    in
        { show1 = showrotation1, symbol1 = symbol1, show2 = showrotation2, symbol2 = symbol2 }


getsymbolfill :
    Base
    -> Rotation
    -> List Fill
    -> Dict String Size
    -> List { fill : Int, symbol : EditorSymbol }
getsymbolfill base rotation validfills symbolsizes =
    List.map
        (\fill ->
            let
                symbol =
                    getSymbolEditorBaseFillRotation base fill rotation symbolsizes
            in
                { symbol = symbol, fill = fill }
        )
        validfills
