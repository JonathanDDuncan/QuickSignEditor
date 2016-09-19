module Layout.LeftSpace exposing (leftspace)

import Html exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import Material.Button as Button
import Material.Icon as Icon


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
            , Button.onClick ShareFsw
            ]
            [ Icon.i "cancel" ]
        , text "This is the leftspace area"
        ]


leftspaceHeight : Model -> String
leftspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"
