module Choosers.State exposing (init, update, subscriptions)

import Choosers.Types
    exposing
        ( Model
        , Msg(..)
        , ChoosingImportModel
        , handsymbolinit
        , chooseriteminit
        )
import Choosers.Types as Editor exposing (Editor)
import Choosers.Types as KeyboardType exposing (KeyboardType)
import Choosers.Types as Loading exposing (Loading)
import Choosers.Types as Hands exposing (Hands)
import Choosers.Types as HandFills exposing (HandFills)
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
        , loadGroupChoosings
        , receiveKeyboardCommand
        , loadPortableSign
        )
import Dict exposing (Dict)
import Material
import Choosers.HandSymbolChooser exposing (createflowersymbols, gethandfillitems)
import SWEditor.EditorSymbol exposing (getSymbolbyBaseFillRotation, getSymbolbyKey, sizeSymbol)
import Update.Extra
import Choosers.HandGroupChooser exposing (gethandgroupchooserdata)
import Choosers.ManiquinKeyboard exposing (runKeyboardCommand)
import Choosers.GroupChooserKeyboard exposing (creategroupchooserkeyboard, totalkeyboardpages)
import Keyboard.Shared exposing (KeyboardMode)
import Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)
import Choosers.Loading exposing (loadingupdate)


init : ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
init =
    ( { lastmdlid = 0
      , mdl = Material.model
      , maniquinchoosings = []
      , clicked = ""
      , selectedcolumn = 1
      , handgroupchoosings = []
      , groupchoosings =
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
            { maniquinkeyboard = []
            , groupchooserkeyboard = []
            , symbolchooserkeyboard = []
            , keyboardpage = 1
            }
      }
    , Cmd.batch [ requestInitialGroupHandChoosings "" ]
    )


update : Choosers.Types.Msg -> Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
update action model =
    case action of
        Noop ->
            ( model
            , Cmd.none
            )

        Mdl msg ->
            Material.update Mdl msg model

        EditorMsg msg ->
            editorupdate msg model

        KeyboardMsg msg ->
            keyboardupdate msg model

        LoadingMsg msg ->
            loadingupdate msg model

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

        SelectHand hand ->
            let
                handsymbol =
                    model.handsymbol

                handfill =
                    case hand of
                        Hands.Left ->
                            case model.handsymbol.handfill of
                                HandFills.RightBack ->
                                    HandFills.LeftBack

                                HandFills.RightThumbEdge ->
                                    HandFills.LeftThumbEdge

                                HandFills.RightPalm ->
                                    HandFills.LeftPalm

                                HandFills.RightBabyEdge ->
                                    HandFills.LeftBabyEdge

                                _ ->
                                    model.handsymbol.handfill

                        Hands.Right ->
                            case model.handsymbol.handfill of
                                HandFills.LeftBack ->
                                    HandFills.RightBack

                                HandFills.LeftThumbEdge ->
                                    HandFills.RightThumbEdge

                                HandFills.LeftPalm ->
                                    HandFills.RightPalm

                                HandFills.LeftBabyEdge ->
                                    HandFills.RightBabyEdge

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
                    |> Update.Extra.andThen update ((KeyboardMsg <| KeyboardType.UpdateChooserKeyboards))


keyboardupdate : KeyboardType -> Model -> ( Model, Cmd Msg )
keyboardupdate action model =
    case action of
        KeyboardType.Keyboard command ->
            runKeyboardCommand model command update

        KeyboardType.SetKeyboardMode mode ->
            let
                num =
                    Keyboard.Shared.getKeyboardModeCode mode
            in
                ( model
                , sendKeyboardMode num
                )

        KeyboardType.UpdateChooserKeyboards ->
            ( updatechooserkeyboard model
            , Cmd.none
            )

        KeyboardType.NextKeyboardPage ->
            nextkeybordpage model


editorupdate : Choosers.Types.Editor -> Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
editorupdate action model =
    case action of
        Editor.SelectedColumn column ->
            ( { model
                | selectedcolumn = column
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)

        Editor.Clicked clickvalue ->
            let
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
                    |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)
                    |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.GroupChooser)

        Editor.GroupSelected choosing ->
            ( { model
                | groupselected = choosing
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update (KeyboardMsg <| KeyboardType.UpdateChooserKeyboards)
                |> Update.Extra.andThen update UpdateHandSymbolChooser
                |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.SymbolChooser)

        Editor.AddSymbol key ->
            let
                editorsymbol =
                    getSymbolbyKey key model.symbolsizes
            in
                ( model
                , cmdAddSymbol editorsymbol
                )
                    |> Update.Extra.andThen update ((KeyboardMsg << KeyboardType.SetKeyboardMode) Keyboard.Shared.SignView)

        Editor.DragSymbol key ->
            let
                editorsymbol =
                    getSymbolbyKey key model.symbolsizes
            in
                ( model
                , cmdDragSymbol editorsymbol
                )

        Editor.ReplaceSymbol key ->
            let
                editorsymbol =
                    getSymbolbyKey key model.symbolsizes
            in
                ( model
                , cmdReplaceSymbol editorsymbol
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


subscriptions : Sub Choosers.Types.Msg
subscriptions =
    Sub.batch
        [ subLoadManiquinChoosings (LoadingMsg << Loading.LoadManiquinChoosings)
        , loadGroupChoosings (LoadingMsg << Loading.LoadGroupChoosings)
        , receiveKeyboardCommand (KeyboardMsg << KeyboardType.Keyboard)
        , loadPortableSign (LoadingMsg << Loading.LoadPortableSign)
        ]
