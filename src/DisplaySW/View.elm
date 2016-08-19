module DisplaySW.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import Svg as Svg exposing (svg)
import Svg.Attributes exposing (..)
import DisplaySW.Types exposing (..)


--import SubDisplaySW.View exposing (root)


root : Model -> Html Msg
root model =
    div []
        [ div []
            [ input [ onInput Change ] []
            , button [ onClick Check ] [ text "Check" ]
            , Svg.svg
                [ version "1.1"
                , x "0"
                , y "0"
                , viewBox "0 0 500 500"
                ]
                [ Svg.g [ transform "translate(100,100)" ]
                    [ --  text' [ class "sym-fill", Svg.Attributes.style "pointer-events:none;font-family:'SignWriting 2010 Filling';font-size:30px;fill:white;" ] [ text "\x1DA3E" ]
                      -- , text' [ class "sym-line", Svg.Attributes.style "pointer-events:none;font-family:'SignWriting 2010';font-size:30px;fill:black;" ] [ text "\x1DA3E" ]
                      Svg.text' [ class "sym-fill", Svg.Attributes.style "pointer-events:none;font-family:'SignWriting 2010 Filling';font-size:30px;fill:white;" ] [ text model.suggestions ]
                    , Svg.text' [ class "sym-line", Svg.Attributes.style "pointer-events:none;font-family:'SignWriting 2010';font-size:30px;fill:black;" ] [ text model.suggestions ]
                    ]
                ]
            ]
        ]



{- polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
   , polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
        , rect
       [ fill "#7FD13B"
       , x "192.99"
       , y "107.392"
       , width "107.676"
       , height "108.167"
       , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
       ]
       []
   , polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
   , polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
   , polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
   , polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
-}
