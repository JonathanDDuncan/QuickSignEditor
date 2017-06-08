module Layout.Footer exposing (stickyFooter)

import Html exposing (Html, div)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import Keyboard.View exposing (..)


stickyFooter : Layout.Types.Model -> Html Layout.Types.Msg
stickyFooter model =
    div
        [ class "footer", style [ ( "height", toString (model.footerheight) ++ "px" ) ] ]
        [ Html.map Keyboard
            (Keyboard.View.root model.keyboard
                model.signbox.signviewkeyboard
                model.mainchooser.chooserskeyboard
                model.window.windowSize.width
                model.footerheight
            )
        ]
