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
            , ( "margin-left", rightspaceMarginLeft model )
            ]
        ]
        [ text "This is the rightspace area"
          -- , mysignBox model
        , App.map Window (WindowSize.View.root model.window)
        ]


rightspaceHeight model =
    if iswidescreen model then
        toString model.containerHeight ++ "px"
    else if ismediumscreen model then
        toString model.containerHeight ++ "px"
    else
        "auto"


rightspaceMarginLeft model =
    if iswidescreen model then
        toString 70 ++ "%"
    else if ismediumscreen model then
        toString 50 ++ "%"
    else
        toString 0



-- mysignBox : Model -> Html Msg
-- mysignBox model =
--     div [ style [ ( "min-width", "300px" ), ( "min-heigth", "300px" ), ( "width", "300px" ), ( "heigth", "300px" ), ( "background-color", "blue" ) ] ]
--         [ text "Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey Hey " ]
