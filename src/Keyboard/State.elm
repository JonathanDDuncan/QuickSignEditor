module Keyboard.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Keyboard.Types exposing (..)
import Keyboard.KeyboardLayouts exposing (..)
import Keyboard.Extra as KeyboardExtra
import Keyboard.Shared exposing (..)
import Ports exposing (..)


-- import SubFeatures.State


init : ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            KeyboardExtra.init
    in
        ( { keyboardlayout = querty
          , keyboarddisplay = fingerspellingQueryAsl
          , keyboardhistory = []
          , keyboardExtraModel = keyboardModel
          , keyList = []
          , keyboardmode = SignView
          }
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )


update : Keyboard.Types.Msg -> Keyboard.Types.Model -> ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
update action model =
    case action of
        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardExtraModel, keyboardCmd ) =
                    KeyboardExtra.update keyMsg model.keyboardExtraModel

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
                    [ Cmd.map KeyboardExtraMsg keyboardCmd
                    , Ports.sendKeyboardCommand keyboardcommand
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
                ( { model | keyboardhistory = (toString n :: model.keyboardhistory) }
                , Cmd.batch
                    [ Cmd.map KeyboardExtraMsg Cmd.none
                    , Ports.sendKeyboardCommand keyboardcommand
                    ]
                )

        Display msg ->
            ( model
            , Cmd.none
            )



--To nest update of Keyboard
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Keyboard.State.update action model


createKeyboardCommand :
    List Int
    -> KeyboardMode
    -> KeyboardCommand
createKeyboardCommand keyList mode =
    let
        shiftPressed =
            isPressedShift keyList

        ctrlPressed =
            isPressedCtrl keyList

        altPressed =
            isPressedAlt keyList

        modecode =
            keyboardModeCode
                |> List.filter (\m -> Tuple.second m == mode)
                |> List.map (\m -> Tuple.first m)
                |> List.head
                |> Maybe.withDefault 1

        keyboardcommand =
            { shiftPressed = shiftPressed
            , ctrlPressed = ctrlPressed
            , altPressed = altPressed
            , mode = modecode
            , keys = keyList
            }
    in
        keyboardcommand


getmode : List Int -> Model -> KeyboardMode
getmode keyList model =
    if isPressedShift keyList && List.any ((==) 2) keyList then
        SignView
    else if isPressedShift keyList && List.any ((==) 3) keyList then
        GeneralChooser
    else if isPressedShift keyList && List.any ((==) 4) keyList then
        GroupChooser
    else if isPressedShift keyList && List.any ((==) 5) keyList then
        SymbolChooser
    else
        model.keyboardmode


isPressedShift : List Int -> Bool
isPressedShift keyList =
    List.any ((==) 42) keyList || List.any ((==) 53) keyList


isPressedCtrl : List Int -> Bool
isPressedCtrl keyList =
    List.any ((==) 54) keyList || List.any ((==) 60) keyList


isPressedAlt : List Int -> Bool
isPressedAlt keyList =
    List.any ((==) 56) keyList || List.any ((==) 58) keyList


subscriptions : Keyboard.Types.Model -> Sub Keyboard.Types.Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg KeyboardExtra.subscriptions
        ]


getKeyList : List Key -> KeyboardExtra.Model -> List Int
getKeyList keys keyboardExtraModel =
    let
        keyExtraList =
            KeyboardExtra.pressedDown keyboardExtraModel

        listExtraCodes =
            List.map (\key -> KeyboardExtra.toCode key) keyExtraList

        keyids =
            List.map (\key -> key.keyId) (List.concatMap (\code -> List.filter (keyhasCode code) keys) listExtraCodes)
    in
        keyids


keyhasCode : Int -> Key -> Bool
keyhasCode code key =
    if (key.code == code) then
        True
    else
        False



-- To nest subscriptions
-- Sub.batch
--       [ SubFeature.State.subscriptions model.subfeatureFieldName |> Sub.map SubFeatureMsg
--       ]
