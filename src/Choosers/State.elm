module Choosers.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Choosers.Types exposing (..)
import Ports exposing (..)
import Choosing.State exposing (..)
import Choosing.Types exposing (..)
import Exts.List exposing (..)
import Dict exposing (..)
import String exposing (..)
import SW.Types exposing (..)
import Material
import SWEditor.EditorSymbol exposing (getSymbolEditorCode, fromEditorSymbol)
import Choosers.HandSymbolChooser exposing (..)
import SWEditor.EditorSymbol exposing (..)
import Update.Extra exposing (..)
import Choosers.HandGroupChooser exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Choosers.GeneralChooserKeyboard exposing (..)
import Choosers.GroupChooserKeyboard exposing (..)
import Choosers.GeneralGroupChooser exposing (creategeneralgroupchooserdata)
import Keyboard.Shared exposing (KeyboardMode)
import Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)


-- import SubMainChoosers.State


init : ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
init =
    ( { lastmdlid = 0
      , mdl = Material.model
      , choosings = [ Tuple.first (Choosing.State.init "S5" 6 8) ]
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
      , generalgroupchooserdata = []
      , chooserskeyboard =
            { generalchooserkeyboard = []
            , groupchooserkeyboard = []
            , symbolchooserkeyboard = []
            , keyboardpage = 1
            }
      }
      -- To initiate Choosers state
      --  { MainChooserFieldName = fst Choosers.State.init
      --  }
    , Cmd.batch [ Ports.requestInitialChoosings "", Ports.requestInitialGroupHandChoosings "" ]
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

        Choosing msg ->
            ( model
            , Cmd.none
            )

        RequestInitialChoosings ->
            ( model
            , Ports.requestInitialChoosings ""
            )

        ReceiveInitialChoosings choosings1 ->
            let
                choosings =
                    List.map (toModel 0) choosings1

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
                , Cmd.none
                )

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

                updatingmodel1 =
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

                generalgroupchooserdata =
                    creategeneralgroupchooserdata updatingmodel1

                updatingmodel2 =
                    { updatingmodel1
                        | generalgroupchooserdata = generalgroupchooserdata
                    }
            in
                ( updatingmodel2
                , Cmd.none
                )
                    |> Update.Extra.andThen update (SetKeyboardMode Keyboard.Shared.GroupChooser)
                    |> Update.Extra.andThen update UpdateChooserKeyboards

        SymbolView msg ->
            ( model
            , Cmd.none
            )

        SignView msg ->
            ( model
            , Cmd.none
            )

        SelectedColumn column ->
            ( { model
                | selectedcolumn = column
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update UpdateChooserKeyboards

        GroupSelected choosing ->
            ( { model
                | groupselected = choosing
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update UpdateChooserKeyboards
                |> Update.Extra.andThen update (SetKeyboardMode Keyboard.Shared.SymbolChooser)
                |> Update.Extra.andThen update UpdateHandSymbolChooser

        AddSymbol symbol ->
            ( model
            , cmdAddSymbol <| fromEditorSymbol symbol
            )
                |> Update.Extra.andThen update (SetKeyboardMode Keyboard.Shared.SignView)

        DragSymbol key ->
            let
                editorsymbol =
                    (getSymbolEditorKey key model.symbolsizes)

                symbol =
                    fromEditorSymbol editorsymbol
            in
                ( model
                , cmdDragSymbol <| symbol
                )

        ReplaceSymbol key ->
            let
                editorsymbol =
                    (getSymbolEditorKey key model.symbolsizes)

                symbol =
                    fromEditorSymbol editorsymbol
            in
                ( model
                , cmdReplaceSymbol <| symbol
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
                    getSymbolEditorBaseFillRotation model.groupselected.base 3 9 model.symbolsizes

                symbolrighthand =
                    getSymbolEditorBaseFillRotation model.groupselected.base 3 1 model.symbolsizes

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
        ]
