module Layout.RightSpace exposing (rightspace)

import Html exposing (Html, div)
import Layout.State exposing (Model, Msg(Choosers), ismediumscreen, iswidescreen)
import Html.Attributes exposing (class, style)
import Choosers.View


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
            [ Html.map Choosers (Choosers.View.root rightspacewidth model.containerHeight model.mainchooser)
            ]


rightspaceHeight : Model -> String
rightspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"
