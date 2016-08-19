module Drawer.Types exposing (..)

-- import SubDrawers.Types

import Material


type alias Model =
    { side : String
    , active : Bool
    , showing : Bool
    , fullwidth : Int
    , alwaysShowpx : Int
    , height : Int
    , mdl : Material.Model
    }


type Msg
    = Show
    | Hide
    | Mdl (Material.Msg Msg)


type alias Mdl1 =
    Material.Model



-- Plus any other types unique to this Drawer
-- Plus any library function to run on the types
