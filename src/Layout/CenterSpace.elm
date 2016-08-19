module Layout.CenterSpace exposing (centerspace)

import Html exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)


centerspace : Model -> Html Msg
centerspace model =
    div
        [ class "centerspace"
        , style
            [ ( "height", toString model.containerHeight ++ "px" )
            , ( "width", centerspaceWidth model )
            , ( "margin-left", centerspaceMarginLeft model )
            ]
        ]
        [ text "This is the centerspace area" ]


centerspaceWidth : Model -> String
centerspaceWidth model =
    if iswidescreen model then
        toString 40 ++ "%"
    else if ismediumscreen model then
        toString 50 ++ "%"
    else
        toString 100 ++ "%"


centerspaceMarginLeft : Model -> String
centerspaceMarginLeft model =
    if iswidescreen model then
        toString 30 ++ "%"
    else if ismediumscreen model then
        toString 0
    else
        toString 0
