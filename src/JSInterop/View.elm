module JSInterop.View exposing (root)

import Html exposing (..)
import Html.Events exposing (..)
import JSInterop.Types exposing (..)
import String
import Debug
 
root : Model -> Html Msg
root model =
  div []
    [ input [ onInput Change ] []
    , button [ onClick Check ] [ text "Check" ]
    , div [] [ text (String.join ", " <| Debug.log "SuggestionsView" model.suggestions) ]
    ]