module DisplaySW.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Svg as Svg exposing (svg)
import Svg.Attributes exposing (..)
import DisplaySW.Types exposing (..)


--import SubDisplaySW.View exposing (root)


root : Model -> Html Msg
root model =
    div []
        [ div []
            [ input [ onInput Change ] []
            , button [ onClick RequestSign ] [ text "RequestSign" ]
            , Svg.svg
                [ version "1.1"
                , x "0"
                , y "0"
                , viewBox "0 0 500 500"
                ]
                (List.map displaySymbol model.suggestions.syms)
            ]
        ]


displaySymbol : Symbol -> Svg.Svg a
displaySymbol symbol =
    Svg.g [ transform <| "translate(" ++ toString (symbol.x - 200) ++ "," ++ toString (symbol.y - 200) ++ ")" ]
        [ Svg.text'
            [ class "sym-fill"
            ]
            [ text symbol.pua ]
        , Svg.text'
            [ class "sym-line"
            ]
            [ text symbol.pua ]
        ]
