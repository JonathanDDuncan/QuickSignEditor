module Layout.Footer exposing (stickyFooter)

import Html exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)


stickyFooter : Model -> Html Msg
stickyFooter model =
    div
        [ class "footer", style [ ( "height", toString (model.footerheight) ++ "px" ) ] ]
        [ text "this is the sticky footer"
        ]
