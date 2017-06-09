module Choosers.Loading exposing (loadingupdate)

import Choosers.Types
    exposing
        ( Model
        , Msg(..)
        , Loading(..)
        , ChoosingImportModel
        , toModel
        , handsymbolinit
        , chooseriteminit
        )
import Ports
    exposing
        ( requestInitialGroupHandChoosings
        , cmdRequestChoosings
        , sendKeyboardMode
        , cmdaddsigntosignview
        , cmdAddSymbol
        , cmdDragSymbol
        , cmdReplaceSymbol
        , subLoadManiquinChoosings
        , receiveInitialGroupHandChoosings
        , receiveKeyboardCommand
        , loadPortableSign
        )
import Exts.List exposing (unique)
import Dict exposing (Dict)
import SW.Types exposing (Size)
import SW.Symbol exposing (Symbol)
import SW.PortableSign exposing (PortableSign)
import SWEditor.EditorSymbol exposing (getSymbolbyBaseFillRotation, getSymbolbyKey, sizeSymbol)
import SWEditor.EditorSign exposing (getSignBounding)
import Helpers.ViewExtra exposing ((=>))
import Choosers.ManiquinKeyboard exposing (updatemaniquinkeyboard)


loadingupdate : Loading -> Model -> ( Model, Cmd msg )
loadingupdate action model =
    case action of
        LoadManiquinChoosings choosings ->
            let
                choosingwithdimentions =
                    getchoosingsdimentions choosings model.symbolsizes

                maniquinchoosings =
                    List.map (toModel 0) choosingwithdimentions
            in
                ( { model
                    | maniquinchoosings = maniquinchoosings
                    , chooserskeyboard = updatemaniquinkeyboard model maniquinchoosings
                  }
                , Cmd.none
                )

        LoadGroupHandChoosings chooserclassification ->
            let
                allgroupchoosings1 =
                    allgroupchoosings chooserclassification

                sizes =
                    Dict.fromList <|
                        List.map (\symbolsize -> .k symbolsize => Size (.w symbolsize) (.h symbolsize)) chooserclassification.symbolsizes
            in
                ( { model
                    | allgroupchoosings = allgroupchoosings1
                    , symbolsizes = sizes
                  }
                , cmdRequestChoosings ""
                )

        LoadPortableSign portablesign ->
            let
                sizedportablesign =
                    sizeportablesign model.symbolsizes portablesign
            in
                ( model, cmdaddsigntosignview sizedportablesign )


getchoosingsdimentions : List ChoosingImportModel -> Dict String Size -> List ChoosingImportModel
getchoosingsdimentions choosings symbolsizes =
    List.map
        (\choosing ->
            { choosing
                | displaySign = sizeportablesign symbolsizes choosing.displaySign
                , valuestoAdd = sizesymbols symbolsizes choosing.valuestoAdd
            }
        )
        choosings


allgroupchoosings :
    { l
        | basechooseritems :
            List
                { j
                    | base : a
                    , colname : String
                    , common : b
                    , feature : String
                    , name : c
                    , rank : d
                    , rowname : String
                    , symbolid : e
                    , symbolkey : f
                    , unicodepua : g
                    , validfills : h
                    , validrotations : i
                    , groupchooser : String
                }
        , chooseritemvalues :
            List
                { k
                    | choosertype : String
                    , name : String
                    , value : Int
                    , symbolgroup : String
                }
    }
    -> List
        { basesymbol : String
        , choosings :
            List
                { base : a
                , col : Int
                , common : b
                , feature : Int
                , groupchooser : Int
                , name : c
                , rank : d
                , row : Int
                , symbolid : e
                , symbolkey : f
                , unicodepua : g
                , validfills : h
                , validrotations : i
                }
        }
allgroupchoosings chooserclassification =
    let
        basesymbols =
            List.sort <| unique <| List.filter (\value -> value /= "") <| List.map (\item -> item.symbolgroup) chooserclassification.chooseritemvalues

        allgroupchoosings1 =
            List.map
                (\basesymbol1 ->
                    { basesymbol = basesymbol1
                    , choosings = getchoosings basesymbol1 chooserclassification.chooseritemvalues chooserclassification.basechooseritems
                    }
                )
                basesymbols
    in
        allgroupchoosings1


getchoosings :
    a
    -> List
        { b
            | choosertype : String
            , symbolgroup : a
            , name : String
            , value : Int
        }
    -> List
        { l
            | base : c
            , colname : String
            , common : d
            , feature : String
            , name : e
            , rank : f
            , rowname : String
            , symbolid : g
            , symbolkey : h
            , unicodepua : i
            , validfills : j
            , validrotations : k
            , groupchooser : String
        }
    -> List
        { base : c
        , col : Int
        , common : d
        , feature : Int
        , groupchooser : Int
        , name : e
        , rank : f
        , row : Int
        , symbolid : g
        , symbolkey : h
        , unicodepua : i
        , validfills : j
        , validrotations : k
        }
getchoosings symbolgroup chooseritemvalues basechooseritems =
    let
        groupchoosers =
            List.sort <|
                unique <|
                    List.map (\item -> item.name) <|
                        List.filter (\item -> item.choosertype == "groupchooser" && item.symbolgroup == symbolgroup) chooseritemvalues

        items =
            List.filter (\basechooseritem -> List.any ((==) basechooseritem.groupchooser) groupchoosers) basechooseritems

        itemsvalues =
            List.filter (\chooseritemvalue -> List.any ((==) chooseritemvalue.choosertype) groupchoosers) chooseritemvalues

        colitemsvalues =
            List.filter (\chooseritemvalue -> chooseritemvalue.choosertype == "colname") chooseritemvalues

        featureitemsvalues =
            List.filter (\chooseritemvalue -> chooseritemvalue.choosertype == "feature") chooseritemvalues

        converted =
            List.map
                (\item ->
                    creategroupchoosing
                        (getchooservalue item.groupchooser chooseritemvalues)
                        itemsvalues
                        colitemsvalues
                        featureitemsvalues
                        item
                )
                items
    in
        converted


creategroupchoosing :
    b
    -> List { a | name : String, value : Int }
    -> List { a1 | name : String, value : Int }
    -> List { a2 | name : String, value : Int }
    -> { l
        | base : c
        , colname : String
        , common : d
        , feature : String
        , name : e
        , rank : f
        , rowname : String
        , symbolid : g
        , symbolkey : h
        , unicodepua : i
        , validfills : j
        , validrotations : k
       }
    -> { base : c
       , col : Int
       , common : d
       , feature : Int
       , groupchooser : b
       , name : e
       , rank : f
       , row : Int
       , symbolid : g
       , symbolkey : h
       , unicodepua : i
       , validfills : j
       , validrotations : k
       }
creategroupchoosing chooservalue itemsvalues colitemsvalues featureitemsvalues item =
    { base = item.base
    , name = item.name
    , symbolid = item.symbolid
    , symbolkey = item.symbolkey
    , unicodepua = item.unicodepua
    , validfills = item.validfills
    , validrotations = item.validrotations
    , groupchooser = chooservalue
    , common = item.common
    , feature = getvalue item.feature featureitemsvalues
    , row = getvalue item.rowname itemsvalues
    , col = getvalue item.colname colitemsvalues
    , rank = item.rank
    }


getvalue : String -> List { a | name : String, value : Int } -> Int
getvalue name itemsvalues =
    default name .value <|
        List.head <|
            List.filter (\item -> item.name == name) itemsvalues


sizeportablesign : Dict String Size -> PortableSign -> PortableSign
sizeportablesign symbolsizes portablesign =
    let
        syms =
            sizesymbols symbolsizes portablesign.syms

        bounds =
            getSignBounding syms
    in
        { portablesign
            | width = bounds.width
            , height = bounds.height
            , syms = syms
        }


sizesymbols : Dict String Size -> List Symbol -> List Symbol
sizesymbols symbolsizes symbols =
    List.map (sizeSymbol symbolsizes) symbols


getchooservalue :
    String
    -> List { a | choosertype : String, name : String, value : number }
    -> number
getchooservalue choosername itemsvalues =
    default choosername .value <|
        List.head <|
            List.filter (\item -> (item.choosertype == "groupchooser") && (item.name == choosername)) itemsvalues


default : String -> (a -> number) -> Maybe a -> number
default text func val =
    case val of
        Just n ->
            func n

        Nothing ->
            if text /= "" then
                Debug.log (text ++ " not found") 0
            else
                0
