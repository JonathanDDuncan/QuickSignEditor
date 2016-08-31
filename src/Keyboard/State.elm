module Keyboard.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Keyboard.Types exposing (..)
import Dict exposing (..)
import String exposing (..)


-- import SubFeatures.State


init : ( Keyboard.Types.Model, Cmd Keyboard.Types.Msg )
init =
    ( { keyboardlayout = querty
      , keycodedictionary = keytext
      }
      -- To initiate Keyboard state
      --  { featureFieldName = fst Keyboard.State.init
      --  }
    , Cmd.none
    )


querty : KeyboardLayout
querty =
    { name = "QWERTY"
    , codes =
        stringtoCodes
            ("192,49,50,51,52,53,54,55,56,57,48,189,187,"
                ++ "81,87,69,82,84,89,85,73,79,80,219,221,220,"
                ++ "65,83,68,70,71,72,74,75,76,186,222,"
                ++ "90,88,67,86,66,78,77,188,190,191"
            )
    }


keytext : Dict Int String
keytext =
    Dict.fromList
        [ ( 48, "0" )
        , ( 49, "1" )
        , ( 50, "2" )
        , ( 51, "3" )
        , ( 52, "4" )
        , ( 53, "5" )
        , ( 54, "6" )
        , ( 55, "7" )
        , ( 56, "8" )
        , ( 57, "9" )
        , ( 65, "A" )
        , ( 66, "B" )
        , ( 67, "C" )
        , ( 68, "D" )
        , ( 69, "E" )
        , ( 70, "F" )
        , ( 71, "G" )
        , ( 72, "H" )
        , ( 73, "I" )
        , ( 74, "J" )
        , ( 75, "K" )
        , ( 76, "L" )
        , ( 77, "M" )
        , ( 78, "N" )
        , ( 79, "O" )
        , ( 80, "P" )
        , ( 81, "Q" )
        , ( 82, "R" )
        , ( 83, "S" )
        , ( 84, "T" )
        , ( 85, "U" )
        , ( 86, "V" )
        , ( 87, "W" )
        , ( 88, "X" )
        , ( 89, "Y" )
        , ( 90, "Z" )
        , ( 186, ";" )
        , ( 187, "=" )
        , ( 188, "," )
        , ( 189, "-" )
        , ( 190, "." )
        , ( 191, "/" )
        , ( 192, "`" )
        , ( 219, "[" )
        , ( 220, "\\" )
        , ( 221, "]" )
        , ( 222, "'" )
        ]


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
