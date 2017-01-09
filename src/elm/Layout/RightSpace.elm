module Layout.RightSpace exposing (rightspace)

import Html exposing (..)
 
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import MainChooser.View


rightspace : Model -> Html Msg
rightspace model =
    let
        rightspacewidth =
            truncate <| toFloat ((100 - model.rightspacemarginleftpercentage) * model.window.windowSize.width) / 100
    in
        div
            [ if iswidescreen model || ismediumscreen model then
                class "rightspace"
              else
                class ""
            , style
                [ ( "height", rightspaceHeight model )
                , ( "width", toString model.rightspacepercentage ++ "%" )
                , ( "margin-left", toString model.rightspacemarginleftpercentage ++ "%" )
                ]
            ]
            [ Html.map MainChooser (MainChooser.View.root model.mainchooser rightspacewidth model.containerHeight)
            ]


rightspaceHeight : Model -> String
rightspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"
