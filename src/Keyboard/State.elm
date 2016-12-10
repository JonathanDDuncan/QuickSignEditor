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
          , keyboardhistory = []
          , keyboardExtraModel = keyboardModel
          , keyList = []
          }
          -- To initiate Keyboard state
          --  { featureFieldName = fst Keyboard.State.init
          --  }
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
            in
                ( { model
                    | keyboardExtraModel = keyboardExtraModel
                    , keyList = keyList
                  }
                , Cmd.map KeyboardExtraMsg keyboardCmd
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


getKeyList : List Key -> KeyboardExtra.Model -> List Int
getKeyList keys keyboardExtraModel =
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
            getKeys keys keyExtraList

        keyList4 =
            List.append keyList3 otherkeys

        keyList =
            keyList4
    in
        keyList


getKeys : List Key -> List KeyboardExtra.Key -> List Int
getKeys keys keyExtraList =
    let
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
