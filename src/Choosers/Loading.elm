module Choosers.Loading exposing (loadingupdate)

import Choosers.Types exposing (Model, Loading(..), ChoosingModel, ChoosingImportModel, BaseChooserItem, ChooserItem, GroupChoosing, ChooserItemValue)
import Ports exposing (cmdRequestChoosings, cmdaddsigntosignview)
import Exts.List exposing (unique)
import List.Extra
import Dict exposing (Dict)
import SW.Types exposing (Size)
import SW.Symbol exposing (Symbol, Base)
import SW.Sign exposing (lastsignid, refreshsign)
import SW.Rectangle exposing (getBounding)
import SW.PortableSign exposing (PortableSign, portableSigntoSign)
import SWEditor.EditorSymbol exposing (sizeSymbol)
import Helpers.ViewExtra exposing ((=>))
import Choosers.ManiquinKeyboard exposing (updatemaniquinkeyboard)
import SW.Identifier exposing (updateId, lastid)


loadingupdate : Loading -> Model -> ( Model, Cmd msg )
loadingupdate action model =
    case action of
        LoadManiquinChoosings choosings ->
            let
                ( maniquinchoosings, maxid ) =
                    sizechoosings model.symbolsizes choosings
                        |> List.map choosingImportModeltoChoosingModel
                        |> updateChoosingModelids model.lastmdlid
            in
                ( { model
                    | maniquinchoosings = maniquinchoosings
                    , chooserskeyboard =
                        updatemaniquinkeyboard model maniquinchoosings
                    , lastmdlid = Maybe.withDefault model.lastmdlid maxid
                  }
                , Cmd.none
                )

        LoadGroupChoosings chooserclassification ->
            let
                choosings =
                    creategroupchoosings chooserclassification

                sizes =
                    createsymbolssizes chooserclassification.symbolsizes
            in
                ( { model
                    | groupchoosings = choosings
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


createsymbolssizes :
    List { a | h : Int, k : String, w : Int }
    -> Dict String Size
createsymbolssizes sizestoload =
    sizestoload
        |> List.map (\symbolsize -> .k symbolsize => Size (.w symbolsize) (.h symbolsize))
        |> Dict.fromList


sizechoosings : Dict String Size -> List ChoosingImportModel -> List ChoosingImportModel
sizechoosings symbolsizes choosings =
    List.map
        (\choosing ->
            { choosing
                | displaySign = sizeportablesign symbolsizes choosing.displaySign
                , valuestoAdd = sizesymbols symbolsizes choosing.valuestoAdd
            }
        )
        choosings


creategroupchoosings :
    { a
        | basechooseritems : List BaseChooserItem
        , chooseritemvalues : List ChooserItemValue
    }
    -> List GroupChoosing
creategroupchoosings chooserclassification =
    chooserclassification.chooseritemvalues
        |> List.map (\item -> item.symbolgroup)
        |> List.filter (\value -> value /= "")
        |> unique
        |> List.sort
        |> List.map
            (\basesymbol ->
                { basesymbol = basesymbol
                , choosings = getgroupchoosings basesymbol chooserclassification.chooseritemvalues chooserclassification.basechooseritems
                }
            )


getgroupchoosings :
    String
    -> List ChooserItemValue
    -> List BaseChooserItem
    -> List ChooserItem
getgroupchoosings symbolgroup chooseritemvalues basechooseritems =
    let
        groupchoosers =
            chooseritemvalues
                |> List.filter (\item -> item.choosertype == "groupchooser" && item.symbolgroup == symbolgroup)
                |> List.map (\item -> item.name)
                |> unique
                |> List.sort

        itemsvalues =
            List.filter
                (\chooseritemvalue ->
                    List.any ((==) chooseritemvalue.choosertype)
                        groupchoosers
                )
                chooseritemvalues

        colitemsvalues =
            List.filter
                (\chooseritemvalue ->
                    chooseritemvalue.choosertype == "colname"
                )
                chooseritemvalues

        featureitemsvalues =
            List.filter
                (\chooseritemvalue ->
                    chooseritemvalue.choosertype == "feature"
                )
                chooseritemvalues
    in
        basechooseritems
            |> List.filter
                (\basechooseritem -> List.any ((==) basechooseritem.groupchooser) groupchoosers)
            |> List.map
                (\item ->
                    creategroupchoosing
                        (getchooservalue item.groupchooser chooseritemvalues)
                        itemsvalues
                        colitemsvalues
                        featureitemsvalues
                        item
                )


creategroupchoosing :
    Int
    -> List { a | name : String, value : Int }
    -> List { a1 | name : String, value : Int }
    -> List { a2 | name : String, value : Int }
    -> BaseChooserItem
    -> ChooserItem
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
            getBounding syms
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


choosingImportModeltoChoosingModel : ChoosingImportModel -> ChoosingModel
choosingImportModeltoChoosingModel importmodel =
    { displaySign = portableSigntoSign importmodel.displaySign
    , valuestoAdd = importmodel.valuestoAdd
    , value = importmodel.value
    , offset = importmodel.offset
    }


updateChoosingModelid :
    ( { c | displaySign : SW.Sign.Sign, valuestoAdd : List { b | id : a } }, d )
    -> ( e, Int )
    -> ( { c | displaySign : SW.Sign.Sign, valuestoAdd : List { b | id : Int } }, Int )
updateChoosingModelid ( choosing, _ ) ( _, maxid ) =
    let
        sign =
            refreshsign maxid choosing.displaySign

        symbols =
            List.indexedMap (updateId <| lastsignid sign)
                choosing.valuestoAdd
    in
        ( { choosing
            | displaySign = sign
            , valuestoAdd = symbols
          }
        , lastid symbols
        )


updateChoosingModelids :
    Int
    -> List
        { a
            | displaySign : SW.Sign.Sign
            , valuestoAdd : List { b | id : Int }
        }
    -> ( List
            { a
                | displaySign : SW.Sign.Sign
                , valuestoAdd : List { b | id : Int }
            }
       , Maybe Int
       )
updateChoosingModelids maxid choosings =
    List.map (\choosing -> ( choosing, maxid )) choosings
        |> List.Extra.scanl1 updateChoosingModelid
        |> \choosings1 ->
            ( List.map
                (\item ->
                    Tuple.first item
                )
                choosings1
            , List.map
                (\item ->
                    Tuple.second item
                )
                choosings1
                |> List.maximum
            )
