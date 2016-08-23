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
            , ( "margin-left", toString model.centerspacemarginleftpercentage ++ "%" )
            ]
        ]
        [ text "This is the centerspace area"
        , App.map SignBox (SWEditor.View.root model.signbox ((model.centerspacepercentage * model.window.windowSize.width) // 100) model.containerHeight)
        ]
