module Layout.Drawer exposing (drawer)

import Html exposing (Html, div)
import Layout.Types exposing (Model, Msg(..), DrawerModel)
import Html.Attributes exposing (class, style)
import Material.Button as Button
import Material.Options as Options
import Material.Icon as Icon


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
                , Options.onClick DrawerHide
                ]
                [ Icon.i "chevron_right" ]
          else
            Button.render
                Mdl
                [ 2 ]
                model.mdl
                [ Button.icon
                , Options.onClick DrawerShow
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
