module Layout.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (href, class, style)
import Layout.Types exposing (..)


--import SubLayout.View exposing (root)

import Material.Button as Button
import Material.Icon as Icon
import WindowSize.View
import Drawer.View
import Debug


root : Model -> Html Msg
root model =
    div []
        [ div
            []
            [ screenlayout model
            ]
        , stickyFooter model
        ]


screenlayout : Model -> Html Msg
screenlayout model =
    if model.window.windowSize.width > model.widescreen then
        widescreen <| Debug.log "WideScreen" model
    else if model.window.windowSize.width > model.mediumscreen then
        mediumscreen <| Debug.log "MediumScreen" model
    else
        smallscreen <| Debug.log "Smallscreen" model


widescreen : Model -> Html Msg
widescreen model =
    div
        []
        [ leftspace model
        , centerspace model
        , rightspace model
        ]


mediumscreen : Model -> Html Msg
mediumscreen model =
    div
        []
        [ centerspace model
        , rightspace model
          -- , App.map RightDrawer (Drawer.View.root model.rightdrawer (leftspace model))
        ]


smallscreen : Model -> Html Msg
smallscreen model =
    div
        []
        [ centerspace model
          -- , App.map RightDrawer
          --     (Drawer.View.root model.rightdrawer
          --         (div
          --             []
          --             [ App.map leftspace model
          --             , App.map rightspace model
          --             ]
          --         )
          --     )
        ]


mysignBox : Model -> Html Msg
mysignBox model =
    div [ style [ ( "min-width", "300px" ), ( "min-heigth", "300px" ), ( "width", "300px" ), ( "heigth", "300px" ), ( "background-color", "blue" ) ] ]
        [ text "Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey " ]


rightspace : Model -> Html Msg
rightspace model =
    div
        [ class "rightspace"
        , style
            [ ( "height", toString model.containerHeight ++ "px" )
            , ( "width", rightspaceWidth model )
            , ( "margin-left", rightspaceMarginLeft model )
            ]
        ]
        [ mysignBox model
        , App.map Window (WindowSize.View.root model.window)
        ]


rightspaceWidth model =
    if model.window.windowSize.width > model.widescreen then
        toString 30 ++ "%"
    else if model.window.windowSize.width > model.mediumscreen then
        toString 50 ++ "%"
    else
        toString 100 ++ "%"


rightspaceMarginLeft model =
    if model.window.windowSize.width > model.widescreen then
        toString 70 ++ "%"
    else if model.window.windowSize.width > model.mediumscreen then
        toString 50 ++ "%"
    else
        toString 0


leftspace : Model -> Html Msg
leftspace model =
    div
        [ class "leftspace"
        , style
            [ ( "height", toString model.containerHeight ++ "px" )
            , ( "width", leftspaceWidth model )
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
        ]


leftspaceWidth model =
    if model.window.windowSize.width > model.widescreen then
        toString 30 ++ "%"
    else if model.window.windowSize.width > model.mediumscreen then
        toString 100 ++ "%"
    else
        toString 100 ++ "%"


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
        []


centerspaceWidth model =
    if model.window.windowSize.width > model.widescreen then
        toString 40 ++ "%"
    else if model.window.windowSize.width > model.mediumscreen then
        toString 50 ++ "%"
    else
        toString 100 ++ "%"


centerspaceMarginLeft model =
    if model.window.windowSize.width > model.widescreen then
        toString 30 ++ "%"
    else if model.window.windowSize.width > model.mediumscreen then
        toString 0
    else
        toString 0


stickyFooter : Model -> Html Msg
stickyFooter model =
    div
        [ class "footer", style [ ( "height", toString (model.footerheight) ++ "px" ) ] ]
        [ text "this is the sticky footer"
        ]



-- containerHeight : Model -> Int
-- containerHeight model =
--     model.window.windowSize.height - model.footerheight
