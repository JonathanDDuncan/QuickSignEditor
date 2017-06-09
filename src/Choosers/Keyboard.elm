module Choosers.Keyboard exposing (keyboardupdate)

import Choosers.Types exposing (Model, Msg, ChoosingImportModel, Editor, Update)
import Choosers.Types as KeyboardType exposing (KeyboardType)
import Choosers.Types exposing (Loading)
import Choosers.Types exposing (Hands)
import Choosers.Types exposing (HandFills)
import Ports exposing (sendKeyboardMode)
import Choosers.ManiquinKeyboard exposing (runKeyboardCommand)
import Choosers.GroupChooserKeyboard exposing (creategroupchooserkeyboard, totalkeyboardpages)
import Keyboard.Shared exposing (KeyboardMode)
import Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)


keyboardupdate : KeyboardType -> Model -> Update -> ( Model, Cmd Msg )
keyboardupdate action model update =
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
