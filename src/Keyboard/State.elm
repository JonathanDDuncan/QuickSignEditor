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
          , keyboardModel = keyboardModel
          , shiftPressed = False
          , controlPressed = False
          , altPressed = False
          , arrows = { x = 0, y = 0 }
          , wasd = { x = 0, y = 0 }
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
                ( keyboardModel, keyboardCmd ) =
                    KeyboardExtra.update keyMsg model.keyboardModel
            in
                ( { model
                    | keyboardModel = keyboardModel
                    , shiftPressed = Debug.log "shiftPressed" <| KeyboardExtra.isPressed KeyboardExtra.Shift keyboardModel
                    , controlPressed = Debug.log "controlPressed" <| KeyboardExtra.isPressed KeyboardExtra.Control keyboardModel
                    , altPressed = Debug.log "altPressed" <| KeyboardExtra.isPressed KeyboardExtra.Alt keyboardModel
                    
                    , arrows = Debug.log "arrows" <| KeyboardExtra.arrows keyboardModel
                    , wasd =Debug.log "wasd" <|  KeyboardExtra.wasd keyboardModel
                    , keyList = Debug.log "keyList" <| KeyboardExtra.pressedDown keyboardModel
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



-- To nest subscriptions
-- Sub.batch
--       [ SubFeature.State.subscriptions model.subfeatureFieldName |> Sub.map SubFeatureMsg
--       ]
