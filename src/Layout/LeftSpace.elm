module Layout.LeftSpace exposing (leftspace)

import Html exposing (Html, div)
import Layout.State exposing (Model, Msg(Mdl, ShareFsw, HideOverlay), iswidescreen, ismediumscreen)
import Html.Attributes exposing (class, style)
import Material.Button as Button
import Material.Grid exposing (grid, cell, Cell, size)
import Material.Options exposing (Style, css, onClick)


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
        [ [ mdlstyle 50
                [ size Material.Grid.All 6 ]
                [ Button.render
                    Mdl
                    [ 1 ]
                    model.mdl
                    [ Button.raised
                    , Button.ripple
                    , Button.colored
                    , onClick ShareFsw
                    ]
                    [ Html.text "Save" ]
                ]
          , mdlstyle 50
                [ size Material.Grid.All 6 ]
                [ Button.render
                    Mdl
                    [ 2 ]
                    model.mdl
                    [ Button.raised
                    , Button.ripple
                    , Button.accent
                    , Button.colored
                    , onClick HideOverlay
                    ]
                    [ Html.text "Cancel" ]
                ]
          ]
            |> grid []
        ]


mdlstyle : Int -> List (Style a) -> List (Html a) -> Cell a
mdlstyle k styling =
    cell <| List.concat [ leftspacestyle k, styling ]


leftspacestyle : Int -> List (Style a)
leftspacestyle h =
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
