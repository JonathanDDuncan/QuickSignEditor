module Layout.CenterSpace exposing (centerspace)

import Html exposing (..)
import Html.App as App exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import SWEditor.View


centerspace : Model -> Html Msg
centerspace model =
    div
        [ class "centerspace"
        , style
            [ ( "height", toString model.containerHeight ++ "px" )
            , ( "width", toString model.centerspacepercentage ++ "%" )
            , ( "margin-left", centerspaceMarginLeft model )
            ]
        ]
        [ text "This is the centerspace area"
        , App.map SignBox (SWEditor.View.root model.signbox 200 300)
        ]


centerspaceMarginLeft : Model -> String
centerspaceMarginLeft model =
    if iswidescreen model then
        toString 30 ++ "%"
    else if ismediumscreen model then
        toString 0
    else
        toString 0
