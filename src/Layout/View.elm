module Layout.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (href, class, style)
import Layout.Types exposing (..)


--import SubLayout.View exposing (root)

import Material.Button as Button
import Material.Icon as Icon
import WindowSize.View
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
        widescreen model
    else if model.window.windowSize.width > model.mediumscreen then
        mediumscreen model
    else
        smallscreen model


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
        , drawer model (leftspace model)
        ]


smallscreen : Model -> Html Msg
smallscreen model =
    div
        []
        [ centerspace model
        , drawer model
            (div
                []
                [ div
                    [ style
                        [ ( "background-color", "orange" ) ]
                    ]
                    [ leftspace model ]
                , div
                    [ style
                        [ ( "background-color", "purple" )
                        ]
                    ]
                    [ rightspace model ]
                ]
            )
        ]


mysignBox : Model -> Html Msg
mysignBox model =
    div [ style [ ( "min-width", "300px" ), ( "min-heigth", "300px" ), ( "width", "300px" ), ( "heigth", "300px" ), ( "background-color", "blue" ) ] ]
        [ text "Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey " ]


rightspace : Model -> Html Msg
rightspace model =
    div
        [ if iswidescreen model || ismediumscreen model then
            class "rightspace"
          else
            class ""
        , style
            [ ( "height", rightspaceHeight model )
            , ( "width", rightspaceWidth model )
            , ( "margin-left", rightspaceMarginLeft model )
            ]
        ]
        [ text "This is the rightspace area"
        , mysignBox model
        , App.map Window (WindowSize.View.root model.window)
        ]


rightspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        "auto"
    else
        "auto"


rightspaceWidth model =
    if iswidescreen model then
        toString 30 ++ "%"
    else if ismediumscreen model then
        toString 50 ++ "%"
    else
        toString 100 ++ "%"


iswidescreen model =
    model.window.windowSize.width > model.widescreen


ismediumscreen model =
    model.window.windowSize.width
        > model.mediumscreen


rightspaceMarginLeft model =
    if iswidescreen model then
        toString 70 ++ "%"
    else if ismediumscreen model then
        toString 50 ++ "%"
    else
        toString 0


leftspace : Model -> Html Msg
leftspace model =
    div
        [ if iswidescreen model then
            class "leftspace"
          else
            class ""
        , style
            [ ( "height", leftspaceHeight model )
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
        , text "This is the leftspace area"
        ]


leftspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"


leftspaceWidth model =
    if iswidescreen model then
        toString 30 ++ "%"
    else if ismediumscreen model then
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
        [ text "This is the centerspace area" ]


centerspaceWidth model =
    if iswidescreen model then
        toString 40 ++ "%"
    else if ismediumscreen model then
        toString 50 ++ "%"
    else
        toString 100 ++ "%"


centerspaceMarginLeft model =
    if iswidescreen model then
        toString 30 ++ "%"
    else if ismediumscreen model then
        toString 0
    else
        toString 0


stickyFooter : Model -> Html Msg
stickyFooter model =
    div
        [ class "footer", style [ ( "height", toString (model.footerheight) ++ "px" ) ] ]
        [ text "this is the sticky footer"
        ]


drawer : Model -> Html Msg -> Html Msg
drawer model child =
    div
        [ class "drawer"
        , style
            [ ( "width", drawerWidth model.rightdrawer )
            , ( "height", toString model.rightdrawer.height ++ "px" )
            ]
        ]
        [ if model.rightdrawer.showing then
            Button.render
                Mdl
                [ 1 ]
                model.mdl
                [ Button.icon
                , Button.onClick DrawerHide
                ]
                [ Icon.i "chevron_right" ]
          else
            Button.render
                Mdl
                [ 2 ]
                model.mdl
                [ Button.icon
                , Button.onClick DrawerShow
                ]
                [ Icon.i "chevron_left" ]
        , if model.rightdrawer.showing then
            child
          else
            div [] []
        ]


drawerWidth : DrawerModel -> String
drawerWidth model =
    if model.active then
        if model.showing then
            toString model.fullwidth ++ "px"
        else
            toString model.alwaysShowpx ++ "px"
    else
        toString 0 ++ "px"



-- containerHeight : Model -> Int
-- containerHeight model =
--     model.window.windowSize.height - model.footerheight
