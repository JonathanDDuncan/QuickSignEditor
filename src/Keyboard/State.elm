module Keyboard.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Keyboard.Types exposing (..)
import String exposing (..)
import Keyboard.KeyboardLayouts exposing (..)


-- import SubFeatures.State


init : ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
init =
    ( { keyboardlayout = querty
      , keyboarddisplay = fingerspellingQueryAsl
      , keycodedictionary = keycodes
      , keyboardhistory = []
      }
      -- To initiate Keyboard state
      --  { featureFieldName = fst Keyboard.State.init
      --  }
    , Cmd.none
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
        FeatureMessage ->
            ( model
            , Cmd.none
            )

        KeyClicked n ->
            ( { model | keyboardhistory =  (toString n :: model.keyboardhistory) }
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
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubFeature.State.subscriptions model.subfeatureFieldName |> Sub.map SubFeatureMsg
--       ]
