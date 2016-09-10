module Layout.Footer exposing (stickyFooter)

import Html exposing (..)
import Html.App as App exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import Keyboard.View exposing (..)
import SWEditor.EditorSign exposing (..)


stickyFooter : Layout.Types.Model -> Html Layout.Types.Msg
stickyFooter model =
    div
        [ class "footer", style [ ( "height", toString (model.footerheight) ++ "px" ) ] ]
        [ App.map Keyboard (Keyboard.View.root model.keyboard <| centerSignSmallest model.signbox.sign)
        ]
