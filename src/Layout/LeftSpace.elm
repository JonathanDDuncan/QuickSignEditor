module Layout.LeftSpace exposing (leftspace)

import Html exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import Material.Button as Button
import Material.Grid exposing (..)
import Material.Options exposing (Style, css)


leftspace : Model -> Html Msg
leftspace model =
    div
        [ if iswidescreen model then
            class "leftspace"
          else
            class ""
        , Html.Attributes.style
            [ ( "height", leftspaceHeight model )
            , ( "width", toString model.leftspacepercentage ++ "%" )
            ]
        ]
        [ Html.text "This is the leftspace area"
        , [ mdlstyle 50
                [ size All 6 ]
                ([ Button.render
                    Mdl
                    [ 1 ]
                    model.mdl
                    [ Button.raised
                    , Button.ripple
                    , Button.colored
                    , Button.onClick ShareFsw
                    ]
                    [ Html.text "Save" ]
                 ]
                )
          , mdlstyle 50
                [ size All 6 ]
                ([ Button.render
                    Mdl
                    [ 2 ]
                    model.mdl
                    [ Button.raised
                    , Button.ripple
                    , Button.accent
                    , Button.colored
                    , Button.onClick HideOverlay
                    ]
                    [ Html.text "Cancel" ]
                 ]
                )
          ]
            |> grid []
        ]


mdlstyle : Int -> List (Style a) -> List (Html a) -> Cell a
mdlstyle k styling =
    cell <| List.concat [ style k, styling ]


style : Int -> List (Style a)
style h =
    [ css "text-sizing" "border-box"
    , css "height" (toString h ++ "px")
    , css "padding-left" "8px"
    , css "padding-top" "4px"
    , css "color" "white"
    ]


leftspaceHeight : Model -> String
leftspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"
