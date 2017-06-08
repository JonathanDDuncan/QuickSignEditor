module Choosers.State exposing (init, update, subscriptions)

import Choosers.Types exposing (Model, Msg(..), Editor(..), Hands(..), HandFills(..), ChoosingImportModel, toModel, handsymbolinit, chooseriteminit)
import Ports exposing (..)
import Exts.List exposing (..)
import Dict exposing (..)
import String exposing (..)
import SW.Types exposing (Size, PortableSign)
import Material
import Choosers.HandSymbolChooser exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.EditorSign exposing (..)
import Update.Extra exposing (..)
import Choosers.HandGroupChooser exposing (..)
import Helpers.ViewExtra exposing (..)
import Choosers.GeneralChooserKeyboard exposing (..)
import Choosers.GroupChooserKeyboard exposing (..)
import Keyboard.Shared exposing (KeyboardMode)
import Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)


init : ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
init =
    ( { lastmdlid = 0
      , mdl = Material.model
      , choosings = []
      , clicked = ""
      , selectedcolumn = 1
      , handgroupchoosings = []
      , allgroupchoosings =
            [ { basesymbol = ""
              , choosings = []
              }
            ]
      , groupselected = chooseriteminit
      , handgroupfilter = 1
      , symbolsizes = Dict.empty
      , handsymbol = handsymbolinit
      , handgroupchooseritems = []
      , chooserskeyboard =
            { generalchooserkeyboard = []
            , groupchooserkeyboard = []
            , symbolchooserkeyboard = []
            , keyboardpage = 1
            }
      }
    , Cmd.batch [ Ports.requestInitialGroupHandChoosings "" ]
    )


update : Choosers.Types.Msg -> Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
update action model =
    case action of
        Noop ->
            ( model
            , Cmd.none
            )

        MainChooserMessage ->
            ( model
            , Cmd.none
            )

        RequestInitialChoosings ->
            ( model
            , Ports.requestInitialChoosings ""
            )

        ReceiveInitialChoosings choosings1 ->
            let
                choosing2 =
                    getchoosingsdimentions choosings1 model.symbolsizes

                choosings =
                    List.map (toModel 0) choosing2

                generalchooserkeyboard =
                    creategeneralchooserkeyboard choosings

                chooserskeyboard1 =
                    model.chooserskeyboard

                chooserskeyboard2 =
                    { chooserskeyboard1 | generalchooserkeyboard = generalchooserkeyboard }
            in
                ( { model
                    | choosings = choosings
                    , chooserskeyboard = chooserskeyboard2
                  }
                , Cmd.none
                )

        ReceiveInitialGroupHandChoosings chooserclassification ->
            let
                allgroupchoosings1 =
                    allgroupchoosings chooserclassification

                sizes =
                    Dict.fromList <|
                        List.map (\symbolsize -> (.k symbolsize) => (Size (.w symbolsize) (.h symbolsize))) chooserclassification.symbolsizes
            in
                ( { model
                    | allgroupchoosings = allgroupchoosings1
                    , symbolsizes = sizes
                  }
                , Ports.requestInitialChoosings ""
                )

        Editor msg ->
            editorupdate msg model

        SymbolView msg ->
            ( model
            , Cmd.none
            )

        SignView msg ->
            ( model
            , Cmd.none
            )

        FilterHandGroup value ->
            let
                updatedFilterHandGroup =
                    { model
                        | handgroupfilter = value
                    }
            in
                ( { model
                    | handgroupfilter = value
                    , handgroupchooseritems = gethandgroupchooserdata updatedFilterHandGroup
                  }
                , Cmd.none
                )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        SelectHand hand ->
            let
                handsymbol =
                    model.handsymbol

                handfill =
                    case hand of
                        Left ->
                            case model.handsymbol.handfill of
                                RightBack ->
                                    LeftBack

                                RightThumbEdge ->
                                    LeftThumbEdge

                                RightPalm ->
                                    LeftPalm

                                RightBabyEdge ->
                                    LeftBabyEdge

                                _ ->
                                    model.handsymbol.handfill

                        Right ->
                            case model.handsymbol.handfill of
                                LeftBack ->
                                    RightBack

                                LeftThumbEdge ->
                                    RightThumbEdge

                                LeftPalm ->
                                    RightPalm

                                LeftBabyEdge ->
                                    RightBabyEdge

                                _ ->
                                    model.handsymbol.handfill

                newhandsymbol =
                    { handsymbol | hand = hand, handfill = handfill }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateHandSymbolChooser

        SelectPlane plane ->
            let
                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol | plane = plane }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateHandSymbolChooser

        SelectHandFill handfill ->
            let
                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol | handfill = handfill }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateHandSymbolChooser

        UpdateHandSymbolChooser ->
            let
                flowersymbols =
                    createflowersymbols model.handsymbol model.groupselected.base model.symbolsizes

                symbollefthand =
                    getSymbolbyBaseFillRotation model.groupselected.base 3 9 model.symbolsizes

                symbolrighthand =
                    getSymbolbyBaseFillRotation model.groupselected.base 3 1 model.symbolsizes

                handfillitems =
                    gethandfillitems model.groupselected.base model.symbolsizes model.handsymbol.hand model.handsymbol.plane

                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol
                        | flowersymbols = flowersymbols
                        , symbollefthand = symbollefthand
                        , symbolrighthand = symbolrighthand
                        , handfillitems = handfillitems
                    }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateChooserKeyboards

        Keyboard command ->
            runKeyboardCommand model command update

        NextKeyboardPage ->
            nextkeybordpage model

        SetKeyboardMode mode ->
            let
                num =
                    Keyboard.Shared.getKeyboardModeCode mode
            in
                ( model
                , sendKeyboardMode num
                )

        UpdateChooserKeyboards ->
            ( updatechooserkeyboard model
            , Cmd.none
            )

        UpdatePortableSignDimentions portablesign ->
            let
                sizedportablesign =
                    setportablesigndimentions model.symbolsizes portablesign
            in
                ( model, cmdaddsigntosignview sizedportablesign )


editorupdate : Choosers.Types.Editor -> Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
editorupdate action model =
    case action of
        SelectedColumn column ->
            ( { model
                | selectedcolumn = column
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update UpdateChooserKeyboards

        Clicked clickvalue ->
            let
                clickval1 =
                    clickvalue

                basesymbol =
                    String.slice 0 4 clickvalue

                updatedclicked =
                    { model
                        | clicked = clickvalue
                    }

                newmodel =
                    case basesymbol of
                        "S14c" ->
                            let
                                handgroupchooseritems =
                                    gethandgroupchooserdata updatedclicked
                            in
                                { updatedclicked
                                    | clicked = clickvalue
                                    , handgroupchooseritems = handgroupchooseritems
                                }

                        _ ->
                            updatedclicked
            in
                ( newmodel
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateChooserKeyboards
                    |> Update.Extra.andThen update (SetKeyboardMode Keyboard.Shared.GroupChooser)

        GroupSelected choosing ->
            ( { model
                | groupselected = choosing
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update UpdateChooserKeyboards
                |> Update.Extra.andThen update UpdateHandSymbolChooser
                |> Update.Extra.andThen update (SetKeyboardMode Keyboard.Shared.SymbolChooser)

        AddSymbol key ->
            let
                editorsymbol =
                    (getSymbolbyKey key model.symbolsizes)
            in
                ( model
                , cmdAddSymbol editorsymbol
                )
                    |> Update.Extra.andThen update (SetKeyboardMode Keyboard.Shared.SignView)

        DragSymbol key ->
            let
                editorsymbol =
                    (getSymbolbyKey key model.symbolsizes)
            in
                ( model
                , cmdDragSymbol editorsymbol
                )

        ReplaceSymbol key ->
            let
                editorsymbol =
                    (getSymbolbyKey key model.symbolsizes)
            in
                ( model
                , cmdReplaceSymbol editorsymbol
                )


getchoosingsdimentions : List ChoosingImportModel -> Dict String Size -> List ChoosingImportModel
getchoosingsdimentions choosings symbolsizes =
    List.map
        (\choosing ->
            { choosing
                | displaySign = setportablesigndimentions symbolsizes choosing.displaySign
                , valuestoAdd = List.map (sizeSymbol symbolsizes) choosing.valuestoAdd
            }
        )
        choosings


setportablesigndimentions : Dict String Size -> PortableSign -> PortableSign
setportablesigndimentions symbolsizes displaySign =
    let
        syms =
            List.map (sizeSymbol symbolsizes) displaySign.syms

        bounds =
            getSignBounding syms
    in
        { displaySign
            | width = bounds.width
            , height = bounds.height
            , syms = syms
        }


updatechooserkeyboard : Choosers.Types.Model -> Choosers.Types.Model
updatechooserkeyboard model =
    let
        groupchooserkeyboard =
            creategroupchooserkeyboard model

        symbolchooserkeyboard =
            createsymbolchooserkeyboard model

        chooserskeyboard1 =
            model.chooserskeyboard

        chooserskeyboard2 =
            { chooserskeyboard1
                | groupchooserkeyboard = groupchooserkeyboard
                , symbolchooserkeyboard = symbolchooserkeyboard
            }

        newmodel =
            { model
                | chooserskeyboard = chooserskeyboard2
            }
    in
        newmodel


nextkeybordpage : Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
nextkeybordpage model =
    let
        totalpages =
            totalkeyboardpages model

        nextpage =
            model.chooserskeyboard.keyboardpage + 1

        page =
            if nextpage > totalpages then
                1
            else
                nextpage

        chooserskeyboard =
            model.chooserskeyboard

        newchooserskeyboard =
            { chooserskeyboard | keyboardpage = page }

        modelpageupdated =
            { model
                | chooserskeyboard = newchooserskeyboard
            }

        newmodel =
            updatechooserkeyboard modelpageupdated
    in
        ( newmodel
        , Cmd.none
        )


allgroupchoosings chooserclassification =
    let
        basesymbols =
            List.sort <| unique <| List.filter (\value -> value /= "") <| List.map (\item -> item.symbolgroup) chooserclassification.chooseritemvalues

        allgroupchoosings1 =
            List.map (\basesymbol1 -> { basesymbol = basesymbol1, choosings = getchoosings basesymbol1 chooserclassification.chooseritemvalues chooserclassification.basechooseritems }) basesymbols
    in
        allgroupchoosings1


getchoosings symbolgroup chooseritemvalues basechooseritems =
    let
        groupchoosers =
            List.sort <| unique <| List.map (\item -> item.name) <| List.filter (\item -> item.choosertype == "groupchooser" && item.symbolgroup == symbolgroup) chooseritemvalues

        items =
            List.filter (\basechooseritem -> List.any ((==) basechooseritem.groupchooser) groupchoosers) basechooseritems

        itemsvalues =
            List.filter (\chooseritemvalue -> List.any ((==) chooseritemvalue.choosertype) groupchoosers) chooseritemvalues

        colitemsvalues =
            List.filter (\chooseritemvalue -> chooseritemvalue.choosertype == "colname") chooseritemvalues

        featureitemsvalues =
            List.filter (\chooseritemvalue -> chooseritemvalue.choosertype == "feature") chooseritemvalues

        converted =
            List.map (\item -> creategroupchoosing (getchooservalue item.groupchooser chooseritemvalues) itemsvalues colitemsvalues featureitemsvalues item) items
    in
        converted


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



--To nest update of Choosers
--  MainChooserMsg action ->
--          lift .MainChooserFieldName (\m x -> { m | MainChooserFieldName = x })  MainChooserMsg Choosers.State.update action model


subscriptions : Choosers.Types.Model -> Sub Choosers.Types.Msg
subscriptions model =
    Sub.batch
        [ receiveInitialChoosings ReceiveInitialChoosings
        , receiveInitialGroupHandChoosings ReceiveInitialGroupHandChoosings
        , receiveKeyboardCommand Keyboard
        , receiveSign UpdatePortableSignDimentions
        ]
