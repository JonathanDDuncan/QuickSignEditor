module Drawer.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Drawer.Types exposing (..)
import Material.Button as Button
import Material.Icon as Icon


--import SubDrawer.View exposing (root)

import Material.Button as Button
import Material.Icon as Icon


root : Model -> Html Msg
root model =
    div [ class "drawer", style [ ( "width", drawerWidth model ), ( "height", toString model.height ++ "px" ) ] ]
        [ Button.render
            Mdl
            [ 1 ]
            model.mdl
            [ Button.icon
            , Button.onClick Hide
            ]
            [ Icon.i "cancel" ]
        , Button.render
            Mdl
            [ 0 ]
            model.mdl
            [ Button.icon
            , Button.onClick Show
            ]
            [ Icon.i "cancel" ]
        ]


drawerWidth : Model -> String
drawerWidth model =
    if model.active then
        if model.showing then
            toString model.fullwidth ++ "px"
        else
            toString model.alwaysShowpx ++ "px"
    else
        toString 0 ++ "px"
