module Layout.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (href, class, style)
import Layout.Types exposing (..)


--import SubLayout.View exposing (root)

import Material.Button as Button
import Material.Icon as Icon
import WindowSize.View

root : Model -> Html Msg
root model =
    div [ class "container" ]
        [ div
            [ class "container" ]
            [ leftspace model
            , centerspace model
            , rightspace model
            ]
        , stickyFooter model
        ]


mysignBox : Model -> Html Msg
mysignBox model =
    div [ style [ ( "min-width", "300px" ), ( "min-heigth", "300px" ), ( "width", "300px" ), ( "heigth", "300px" ), ( "background-color", "blue" ) ] ]
        [ text "Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey " ]


rightspace : Model -> Html Msg
rightspace model =
    div [ class "rightspace", style [ ( "height", toString (model.window.windowSize.height - model.footerheight) ++ "px" ) ] ]
        [ mysignBox model
        , App.map Window (WindowSize.View.root model.window) 
        ]


leftspace : Model -> Html Msg
leftspace model =
    div [ class "leftspace", style [ ( "height", toString (model.window.windowSize.height - model.footerheight) ++ "px" ) ] ]
        [ Button.render
            Mdl
            [ 0 ]
            model.mdl
            [ Button.icon
            , Button.onClick HideOverlay
            ]
            [ Icon.i "cancel" ]
        ]


centerspace : Model -> Html Msg
centerspace model =
    div [ class "centerspace", style [ ( "height", toString (model.window.windowSize.height - model.footerheight) ++ "px" ) ] ]
        []


stickyFooter : Model -> Html Msg
stickyFooter model =
    div
        [ class "footer" ,style [ ( "height", toString (  model.footerheight) ++ "px" ) ]]
        [ text "this is the sticky footer"
        ]
