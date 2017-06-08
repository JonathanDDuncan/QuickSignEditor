module Keyboard.State exposing (init, update, subscriptions)

import Keyboard.Types exposing (Model, Msg(..), Key)
import Keyboard.KeyboardLayouts exposing (querty, fingerspellingQueryAsl)
import Keyboard.Extra
import Keyboard.Shared exposing (KeyboardMode, createKeyboardCommand, getKeyboardMode, isPressedShift)
import Keyboard.Shared as KeyboardMode exposing (KeyboardMode)
import Ports exposing (sendKeyboardCommand, receiveKeyboardMode)


init : ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
init =
    ( { keyboardlayout = querty
      , keyboarddisplay = fingerspellingQueryAsl
      , keyboardhistory = []
      , keyboardExtraModel = Keyboard.Extra.initialState
      , keyList = []
      , keyboardmode = KeyboardMode.GeneralChooser
      }
    , Cmd.none
    )


update : Keyboard.Types.Msg -> Keyboard.Types.Model -> ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
update action model =
    case action of
        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardExtraModel, keyboardCmd ) =
                    Keyboard.Extra.updateWithKeyChange keyMsg model.keyboardExtraModel

                keyList =
                    getKeyList model.keyboardlayout.keys keyboardExtraModel

                newmode =
                    getmode keyList model

                keyboardcommand =
                    createKeyboardCommand keyList newmode
            in
                ( { model
                    | keyboardExtraModel = keyboardExtraModel
                    , keyList = keyList
                    , keyboardmode = newmode
                  }
                , Cmd.batch
                    [ sendKeyboardCommand keyboardcommand
                    ]
                )

        KeyClicked n ->
            let
                keyList =
                    [ n ]

                newmode =
                    getmode keyList model

                keyboardcommand =
                    createKeyboardCommand keyList newmode
            in
                ( { model | keyboardhistory = toString n :: model.keyboardhistory }
                , Cmd.batch
                    [ Cmd.map KeyboardExtraMsg Cmd.none
                    , Ports.sendKeyboardCommand keyboardcommand
                    ]
                )

        DisplaySignView _ ->
            ( model
            , Cmd.none
            )

        DisplayChoosers _ ->
            ( model
            , Cmd.none
            )

        SetKeyboardMode num ->
            ( { model | keyboardmode = getKeyboardMode num }
            , Cmd.none
            )



--To nest update of Keyboard
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Keyboard.State.update action model


getmode : List Int -> Model -> KeyboardMode
getmode keyList model =
    if isPressedShift keyList && List.any ((==) 2) keyList then
        KeyboardMode.GeneralChooser
    else if isPressedShift keyList && List.any ((==) 3) keyList then
        KeyboardMode.GroupChooser
    else if isPressedShift keyList && List.any ((==) 4) keyList then
        KeyboardMode.SymbolChooser
    else if isPressedShift keyList && List.any ((==) 5) keyList then
        KeyboardMode.SignView
    else
        model.keyboardmode


subscriptions : Sub Keyboard.Types.Msg
subscriptions =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , receiveKeyboardMode SetKeyboardMode
        ]


getKeyList : List Key -> Keyboard.Extra.State -> List Int
getKeyList keys keyboardExtraModel =
    let
        keyExtraList =
            Keyboard.Extra.pressedDown keyboardExtraModel

        listExtraCodes =
            List.map (\key -> Keyboard.Extra.toCode key) keyExtraList

        keyids =
            List.map (\key -> key.keyId) (List.concatMap (\code -> List.filter (keyhasCode code) keys) listExtraCodes)
    in
        keyids


keyhasCode : Int -> Key -> Bool
keyhasCode code key =
    if key.code == code then
        True
    else
        False



-- To nest subscriptions
-- Sub.batch
--       [ SubFeature.State.subscriptions model.subfeatureFieldName |> Sub.map SubFeatureMsg
--       ]
