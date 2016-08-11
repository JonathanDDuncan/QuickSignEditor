module Clock.View exposing (root)

import Html exposing (Html)
import Clock.Types exposing (..)
import Time exposing (Time, second)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- You may need these latter
-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)
-- import Set
-- import String
-- import feature types and sub views
-- import Widget.Types
-- import Widget.View
-- import Grommit.Types
-- import Grommit.View


root : Clock.Types.Model -> Html Msg
root model =
    let
        angle =
            turns (Time.inMinutes model)

        handX =
            toString (50 + 40 * (cos (angle)))

        handY =
            toString (50 + 40 * sin angle)
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
            , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
            ]
