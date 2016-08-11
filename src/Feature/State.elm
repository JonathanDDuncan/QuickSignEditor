module Feature.State exposing (..)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)

import Feature.Types exposing (..)


initialState : ( Feature.Types.Model, Cmd Feature.Types.Msg )
initialState =
    ( { field = 0
      }
    , Cmd.none
      -- map commands
      -- Cmd.map LoadNews loadNewsStories
    )



-- this is another example of init
-- init : Time -> ( Model, Cmd Msg )
-- init startTime = ...


subscriptions : Feature.Types.Model -> Sub Feature.Types.Msg
subscriptions _ =
    Sub.none


update : Feature.Types.Msg -> Feature.Types.Model -> ( Feature.Types.Model, Cmd Feature.Types.Msg )
update action model =
    case action of
        FeatureMessage ->
            ( { model | field = 0 }
            , Cmd.none
            )
