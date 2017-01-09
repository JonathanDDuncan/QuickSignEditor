module Types exposing (..)

-- import Feature.Types

import Overlay.Types


type alias Model =
    { overlay :
        Overlay.Types.Model
        --Nest feature object to model by adding
        -- featureFieldName : Feature.Types.Model
    }


type Msg
    = Overlay Overlay.Types.Msg



-- Nest feature messages by adding
-- | FeatureMsg Feature.Types.Msg
