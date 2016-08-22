module DisplaySW.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Svg as Svg exposing (svg)
import Svg.Attributes exposing (..)
import DisplaySW.Types exposing (..)
import SW.Types exposing (..)


--import SubDisplaySW.View exposing (root)


root : Model -> Html Msg
root model =
    div []
        [ div []
            [ input [ onInput Change ] []
            , button [ onClick RequestSign ] [ text "RequestSign" ]
            , signView model.sign
            ]
        ]


signView : Sign -> Svg.Svg a
signView sign =
    Svg.svg
        [ version "1.1"
        , width <| toString sign.width
        , height <| toString sign.height
        , viewBox (toString (500 - sign.width // 2) ++ " " ++ toString (500 - sign.height // 2) ++ " " ++ toString sign.width ++ " " ++ toString sign.height)
        ]
        (List.map symbolView sign.syms)


symbolView : Symbol -> Svg.Svg a
symbolView symbol =
    Svg.g [ transform <| "translate(" ++ toString (symbol.x) ++ "," ++ toString (symbol.y) ++ ")" ]
        [ Svg.text'
            [ class "sym-fill"
            ]
            [ text symbol.pua ]
        , Svg.text'
            [ class "sym-line"
            ]
            [ text symbol.pua ]
        ]
