module Jumbotron.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App exposing (..)
import Jumbotron.Types exposing (..)
import Hello.View exposing (..)


root : Model -> Html Msg
root model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ -- inline CSS (literal)
          div [ class "row" ]
            [ div
                [ class "col-xs-12"
                ]
                [ div [ class "jumbotron" ]
                    [ img [ src "img/elm.jpg", style styles.img ] []
                      -- inline CSS (via var)
                    , App.map Hello (Hello.View.root model.hello)
                    ]
                ]
            ]
        ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }
