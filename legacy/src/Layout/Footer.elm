module Layout.Footer exposing (stickyFooter)

import Html exposing (Html, div)
import Layout.State exposing (Model, Msg(Keyboard))
import Html.Attributes exposing (class, style)
import Keyboard.View


stickyFooter : Model -> Html Msg
stickyFooter model =
    div
        [ class "footer", style [ ( "height", toString model.footerheight ++ "px" ) ] ]
        [ Html.map Keyboard
            (Keyboard.View.root model.keyboard
                model.signbox.signviewkeyboard
                model.mainchooser.chooserskeyboard
                model.window.windowSize.width
                model.footerheight
            )
        ]
