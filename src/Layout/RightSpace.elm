module Layout.RightSpace exposing (rightspace)

import Html exposing (..)
import Html.App as App exposing (..)
import Layout.Types exposing (..)
import Html.Attributes exposing (href, class, style)
import WindowSize.View


rightspace : Model -> Html Msg
rightspace model =
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
        [ text "This is the rightspace area"
          -- , mysignBox model
        , App.map Window (WindowSize.View.root model.window)
        ]


rightspaceHeight : Model -> String
rightspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"
