module Layout.View exposing (root)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Layout.State exposing (Model, Msg)
import Layout.CenterSpace exposing (centerspace)
import Layout.LeftSpace exposing (leftspace)
import Layout.RightSpace exposing (rightspace)
import Layout.Footer exposing (stickyFooter)
import Layout.Drawer exposing (drawer)
import Layout.AboutDialog exposing (aboutdialog)


root : Model -> Html Msg
root model =
    div []
        [ aboutdialog model
        , div
            []
            [ screenlayout model
            ]
        , stickyFooter model
        ]


screenlayout : Model -> Html Msg
screenlayout model =
    if model.window.windowSize.width > model.widescreenwidth then
        widescreen model
    else if model.window.windowSize.width > model.mediumscreenwidth then
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
                        [ ( "background-color", "lightsteelblue" ) ]
                    ]
                    [ leftspace model ]
                , div
                    [ style
                        [ ( "background-color", "lightyellow" )
                        ]
                    ]
                    [ rightspace model ]
                ]
            )
        ]
