module Keyboard.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Types exposing (..)
import Array exposing (..)
import String
import Json.Decode as Json
import Keyboard.Shared exposing (..)
import SWEditor.Types exposing (Msg)
import MainChooser.Types exposing (Msg)


--import SubFeature.View exposing (root)


text3 : List { display : Html Keyboard.Types.Msg, test : { key : Int, special : List Int } }
text3 =
    [ { test = { key = 67, special = [] }
      , display = text "43"
      }
    , { test = { key = 69, special = [] }
      , display = text "43"
      }
    , { test = { key = 70, special = [] }
      , display = text "43"
      }
    , { test = { key = 68, special = [] }
      , display = text "43"
      }
    ]


getdisplay : { display : Html Keyboard.Types.Msg, test : { key : Int, special : List Int } } -> Html Keyboard.Types.Msg
getdisplay cfg =
    cfg.display



-- text2 : Html msg
-- text2 =
--     text "text2"


root :
    Model
    -> List (Keyboard.Shared.KeyConfig SWEditor.Types.Msg)
    -> { generalchooserkeyboard :
            List (Keyboard.Shared.KeyConfig MainChooser.Types.Msg)
       , groupchooserkeyboard :
            List (Keyboard.Shared.KeyConfig MainChooser.Types.Msg)
       , symbolchooserkeyboard :
            List (Keyboard.Shared.KeyConfig MainChooser.Types.Msg)
       }
    -> Int
    -> Html Keyboard.Types.Msg
root model signviewkeyboard chooserskeyboard footerwidth =
    let
        keyboarddisplay =
            case model.keyboardmode of
                SignView ->
                    -- signviewkeyboard
                    convertotkeyboardmsg chooserskeyboard.generalchooserkeyboard

                GeneralChooser ->
                    convertotkeyboardmsg chooserskeyboard.generalchooserkeyboard

                GroupChooser ->
                    convertotkeyboardmsg chooserskeyboard.groupchooserkeyboard

                SymbolChooser ->
                    convertotkeyboardmsg chooserskeyboard.symbolchooserkeyboard

        -- text1 =
        --     List.head text3
        --         |> Maybe.withDefault
        --             { test = { key = 67, special = [] }
        --             , display = text "43"
        --             }
    in
        div [ class "keyboard" ]
            [ text (String.concat model.keyboardhistory)
            , div
                [ class "alphabetic", style [ ( "width", "66%" ) ] ]
                [ row model (List.range 1 14) keyboarddisplay footerwidth
                , row model (List.range 15 28) keyboarddisplay footerwidth
                , row model (List.range 29 41) keyboarddisplay footerwidth
                , row model (List.range 42 53) keyboarddisplay footerwidth
                , row model (List.range 54 60) keyboarddisplay footerwidth
                ]
            , div
                [ class "arrows", style [ ( "width", "14%" ) ] ]
                [ div [ style [ ( "height", "40%" ), ( "margin-bottom", "15.5%" ) ] ]
                    [ row model (List.range 61 63) keyboarddisplay footerwidth
                    , row model (List.range 64 66) keyboarddisplay footerwidth
                    ]
                , div [ style [ ( "height", "40%" ) ] ]
                    [ row model (List.range 67 67) keyboarddisplay footerwidth
                    , row model (List.range 68 70) keyboarddisplay footerwidth
                    ]
                ]
            , div
                [ class "numeric", style [ ( "width", "20%" ) ] ]
                [ row model (List.range 71 73) keyboarddisplay footerwidth
                , row model (List.range 74 77) keyboarddisplay footerwidth
                , row model (List.range 78 80) keyboarddisplay footerwidth
                , row model (List.range 81 83) keyboarddisplay footerwidth
                , row model (List.range 84 86) keyboarddisplay footerwidth
                ]
            ]


convertotkeyboardmsg :
    List
        { a
            | display : Html MainChooser.Types.Msg
            , test : KeyTestConfig
        }
    -> List
        { display : Html Keyboard.Types.Msg
        , test : KeyTestConfig
        }
convertotkeyboardmsg keyboarddisplay =
    List.map
        (\config ->
            { display =
                Html.map (DisplayChoosers) config.display
            , test = config.test
            }
        )
        keyboarddisplay


row : Model -> List Int -> List { display : Html Keyboard.Types.Msg, test : KeyTestConfig } -> Int -> Html Keyboard.Types.Msg
row model nums display footerwidth =
    div [ class "row" ]
        (createkeys model nums display footerwidth)


createkeys : Model -> List Int -> List { display : Html Keyboard.Types.Msg, test : KeyTestConfig } -> Int -> List (Html Keyboard.Types.Msg)
createkeys model nums display footerwidth =
    List.map (\n -> nkey model n display footerwidth) nums


nkey : Model -> Int -> List { display : Html Keyboard.Types.Msg, test : KeyTestConfig } -> Int -> Html Keyboard.Types.Msg
nkey model n display footerwidth =
    let
        leftmargin =
            17

        size =
            { height = 20, width = 20 }

        scale =
            calcscale size 30 ((minkeywidth footerwidth) - leftmargin)

        width =
            round (toFloat size.width * scale)

        ispressed =
            checkifkeypressed model n

        pressed =
            if (ispressed) then
                " pressed"
            else
                ""

        activemode =
            if (isactivemode n model) then
                " activemode"
            else
                ""
    in
        div [ class <| "key k" ++ toString n, onClick (KeyClicked n), onTouchEnd (KeyClicked n) ]
            [ div [ class <| "scaletoparent" ++ pressed ++ activemode ]
                [ -- text2
                  (Maybe.withDefault { display = text "this", test = { key = 69, special = [] } } <| List.head display).display
                  -- , Html.map
                  --     Display
                  --     (SWEditor.Display.scaledSignView display scale leftmargin)
                ]
            , span [] [ text <| getKeyDisplay n model ]
            ]


isactivemode : number -> { a | keyboardmode : KeyboardMode } -> Bool
isactivemode n model =
    case model.keyboardmode of
        SignView ->
            n == 2

        GeneralChooser ->
            n == 3

        GroupChooser ->
            n == 4

        SymbolChooser ->
            n == 5


checkifkeypressed : Model -> Int -> Bool
checkifkeypressed model n =
    List.any ((==) n) model.keyList


minkeywidth : Int -> Float
minkeywidth footerwidth =
    toFloat footerwidth * 0.66 * 0.063 - 3


calcscale : { a | height : Int, width : Int } -> Float -> Float -> Float
calcscale sign height width =
    let
        availableWidth =
            width

        contentWidth =
            sign.width

        availableHeight =
            height

        contentHeight =
            sign.height

        scale =
            Basics.min (availableWidth / toFloat contentWidth) (availableHeight / toFloat contentHeight)
    in
        scale


getKeyDisplay : Int -> Model -> String
getKeyDisplay n model =
    let
        keycode =
            Maybe.withDefault { keyId = 0, code = 0, display = "", keypress = None } (nth n model.keyboardlayout.keys)
    in
        keycode.display


nth : Int -> List a -> Maybe a
nth i list =
    Array.get i (Array.fromList list)


onTouchStart : msg -> Attribute msg
onTouchStart message =
    on "touchstart" (Json.succeed message)


onTouchEnd : msg -> Attribute msg
onTouchEnd message =
    on "touchend" (Json.succeed message)
