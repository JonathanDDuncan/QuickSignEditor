module Layout.CenterSpace exposing (centerspace)

import Html exposing (..)
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
        [ Html.map SWEditor
            (SWEditor.View.root model.signbox)
        ]
