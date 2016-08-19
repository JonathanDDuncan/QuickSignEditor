module Layout.Types exposing (..)

-- import SubLayouts.Types

import Material


type alias Model =
    { count : Int
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


type Msg
    = Increase
    | Reset
    | Mdl (Material.Msg Msg)


type alias Mdl =
    Material.Model



-- Plus any other types unique to this Layout
-- Plus any library function to run on the types
