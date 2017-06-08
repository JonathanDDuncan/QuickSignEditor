module Types exposing (Model, Msg(..))

-- import Feature.Types

import Overlay.State


type alias Model =
    { overlay :
        Overlay.State.Model
        --Nest feature object to model by adding
        -- featureFieldName : Feature.Types.Model
    }


type Msg
    = Overlay Overlay.State.Msg



-- Nest feature messages by adding
-- | FeatureMsg Feature.Types.Msg
