module Keyboard.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Keyboard.Types exposing (..)
import String exposing (..)
import Keyboard.KeyboardLayouts exposing (..)
import Keyboard.Extra as KeyboardExtra


-- import SubFeatures.State


init : ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            KeyboardExtra.init
    in
        ( { keyboardlayout = querty
          , keyboarddisplay = fingerspellingQueryAsl
          , keycodedictionary = keycodes
          , keyboardhistory = []
          , keyboardExtraModel = keyboardModel
          , shiftPressed = False
          , controlPressed = False
          , altPressed = False
          , arrows = { x = 0, y = 0 }
          , wasd = { x = 0, y = 0 }
          , keyExtraList = []
          , keyList = []
          }
          -- To initiate Keyboard state
          --  { featureFieldName = fst Keyboard.State.init
          --  }
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )


stringtoCodes : String -> List Int
stringtoCodes str =
    List.map
        (\s ->
            let
                result =
                    toInt s
            in
                case result of
                    Ok value ->
                        value

                    Err msg ->
                        -1
        )
        (split "," str)


update : Keyboard.Types.Msg -> Keyboard.Types.Model -> ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
update action model =
    case action of
        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardExtraModel, keyboardCmd ) =
                    KeyboardExtra.update keyMsg model.keyboardExtraModel

                keyExtraList =
                    Debug.log "keyList" <| KeyboardExtra.pressedDown keyboardExtraModel

                shiftPressed =
                    Debug.log "shiftPressed" <| KeyboardExtra.isPressed KeyboardExtra.Shift keyboardExtraModel

                controlPressed =
                    Debug.log "controlPressed" <| KeyboardExtra.isPressed KeyboardExtra.Control keyboardExtraModel

                altPressed =
                    Debug.log "altPressed" <| KeyboardExtra.isPressed KeyboardExtra.Alt keyboardExtraModel

                keyList =
                    getKeyList model keyboardExtraModel
            in
                ( { model
                    | keyboardExtraModel = keyboardExtraModel
                    , shiftPressed = shiftPressed
                    , controlPressed = controlPressed
                    , altPressed = altPressed
                    , arrows = Debug.log "arrows" <| KeyboardExtra.arrows keyboardExtraModel
                    , wasd = Debug.log "wasd" <| KeyboardExtra.wasd keyboardExtraModel
                    , keyExtraList = keyExtraList
                    , keyList = keyList
                  }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

        FeatureMessage ->
            ( model
            , Cmd.none
            )

        KeyClicked n ->
            ( { model | keyboardhistory = (toString n :: model.keyboardhistory) }
            , Cmd.none
            )

        Display msg ->
            ( model
            , Cmd.none
            )



--To nest update of Keyboard
--  FeatureMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  FeatureMsg Keyboard.State.update action model


subscriptions : Keyboard.Types.Model -> Sub Keyboard.Types.Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg KeyboardExtra.subscriptions
        ]


getKeyList model keyboardExtraModel =
    let
        shiftPressed =
            KeyboardExtra.isPressed KeyboardExtra.Shift keyboardExtraModel

        controlPressed =
            KeyboardExtra.isPressed KeyboardExtra.Control keyboardExtraModel

        altPressed =
            KeyboardExtra.isPressed KeyboardExtra.Alt keyboardExtraModel

        keyExtraList =
            KeyboardExtra.pressedDown keyboardExtraModel

        keyListEmpty =
            []

        keyList1 =
            if (shiftPressed) then
                [ 42, 53 ]
            else
                keyListEmpty

        keyList2 =
            if (controlPressed) then
                List.append keyList1 [ 54, 60 ]
            else
                keyList1

        keyList3 =
            if (altPressed) then
                List.append keyList2 [ 56, 58 ]
            else
                keyList2

        otherkeys =
            getKeys model keyExtraList

        keyList4 =
            List.append keyList3 otherkeys

        keyList =
            keyList4
    in
        keyList


getKeys model keyExtraList =
    let
        listExtraCodes =
            List.map (\key -> KeyboardExtra.toCode key) keyExtraList

        keyids =
            Debug.log "keyids" <| 
             List.map (\key ->  key.keyId)  (List.concatMap (\code -> List.filter (keyhasCode code) model.keyboardlayout.keys) listExtraCodes)
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
