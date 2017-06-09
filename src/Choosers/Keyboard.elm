module Choosers.Keyboard exposing (keyboardupdate)

import Choosers.Types exposing (Model, Msg, ChoosingImportModel, Editor, Update)
import Choosers.Types as KeyboardType exposing (KeyboardType)
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
            ( model
            , sendKeyboardMode <| Keyboard.Shared.getKeyboardModeCode mode
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
        chooserskeyboard =
            model.chooserskeyboard
    in
        { model
            | chooserskeyboard =
                { chooserskeyboard
                    | groupchooserkeyboard = creategroupchooserkeyboard model
                    , symbolchooserkeyboard = createsymbolchooserkeyboard model
                }
        }


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
    in
        ( updatechooserkeyboard
            { model
                | chooserskeyboard = { chooserskeyboard | keyboardpage = page }
            }
        , Cmd.none
        )
