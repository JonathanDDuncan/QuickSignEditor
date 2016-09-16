module Layout.LeftSpace exposing (leftspace)

import Html exposing (..)
import Html.App as App exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import Material.Button as Button
import Material.Icon as Icon
import SWEditor.Display exposing (signView)
import SWEditor.EditorSign exposing (..)


leftspace : Model -> Html Msg
leftspace model =
    div
        [ if iswidescreen model then
            class "leftspace"
          else
            class ""
        , style
            [ ( "height", leftspaceHeight model )
            , ( "width", toString model.leftspacepercentage ++ "%" )
            ]
        ]
        [ Button.render
            Mdl
            [ 0 ]
            model.mdl
            [ Button.icon
            , Button.onClick HideOverlay
            ]
            [ Icon.i "cancel" ]
        , text "This is the leftspace area"
        , div [ class "key" ] [ div [ class "scaletoparent" ] [ App.map SWEditor (SWEditor.Display.signView (centerSignSmallest model.signbox.sign) []) ] ]
        ]


leftspaceHeight : Model -> String
leftspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"
