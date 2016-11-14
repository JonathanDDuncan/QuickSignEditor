module Layout.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Layout.Types exposing (..)


--import SubLayout.View exposing (root)

import Layout.CenterSpace exposing (..)
import Layout.LeftSpace exposing (..)
import Layout.RightSpace exposing (..)
import Layout.Footer exposing (..)
import Layout.Drawer exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ div
            []
            [ screenlayout model
            ]
        , stickyFooter model
        ]


screenlayout : Model -> Html Msg
screenlayout model =
    if  model.window.windowSize.width > model.widescreenwidth then
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
                        [ ( "background-color", "orange" ) ]
                    ]
                    [ leftspace model ]
                , div
                    [ style
                        [ ( "background-color", "purple" )
                        ]
                    ]
                    [ rightspace model ]
                ]
            )
        ]



-- containerHeight : Model -> Int
-- containerHeight model =
--     model.window.windowSize.height - model.footerheight
