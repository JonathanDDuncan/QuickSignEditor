module Layout.Types exposing (..)

-- import SubLayouts.Types

import Material
import WindowSize.Types


type alias Model =
    { count : Int
    , mdl : Material.Model
    , window : WindowSize.Types.Model
    , rightdrawer : DrawerModel
    , footerheight :
        Int
    , containerHeight :
        Int
    , widescreen : Int
    , mediumscreen :
        Int
        -- Boilerplate: model store for any and all Mdl components you use.
    }


type Msg
    = Increase
    | Reset
    | HideOverlay
    | Window WindowSize.Types.Msg
    | DrawerShow
    | DrawerHide
    | Mdl (Material.Msg Msg)


type alias Mdl =
    Material.Model


type alias DrawerModel =
    { active : Bool
    , showing : Bool
    , fullwidth : Int
    , alwaysShowpx : Int
    , height : Int
    }



-- Plus any other types unique to this Layout
-- Plus any library function to run on the types


iswidescreen model =
    model.window.windowSize.width > model.widescreen


ismediumscreen model =
    model.window.windowSize.width
        > model.mediumscreen
