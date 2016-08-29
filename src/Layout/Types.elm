module Layout.Types exposing (..)

-- import SubLayouts.Types

import Material
import WindowSize.Types
import SWEditor.Types


type alias Model =
    { count : Int
    , mdl : Material.Model
    , window : WindowSize.Types.Model
    , signbox : SWEditor.Types.Model
    , rightdrawer : DrawerModel
    , footerheight : Int
    , containerHeight : Int
    , widescreenwidth : Int
    , mediumscreenwidth : Int
    , rightspacepercentage : Int
    , centerspacepercentage : Int
    , leftspacepercentage : Int
    , rightspacemarginleftpercentage : Int
    , centerspacemarginleftpercentage : Int
    , drawerwidth : Int
    }



-- Model mdl Boilerplate: model store for any and all Mdl components you use.


type Msg
    = Increase
    | Reset
    | HideOverlay
    | Window WindowSize.Types.Msg
    | SWEditor SWEditor.Types.Msg
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


iswidescreen : Model -> Bool
iswidescreen model =
    iswidescreenexplicit model.window.windowSize.width model.widescreenwidth


iswidescreenexplicit : Int -> Int -> Bool
iswidescreenexplicit width widescreenwidth =
    width > widescreenwidth


ismediumscreen : Model -> Bool
ismediumscreen model =
    ismediumscreenexplicit model.window.windowSize.width model.mediumscreenwidth


ismediumscreenexplicit : Int -> Int -> Bool
ismediumscreenexplicit width mediumscreenwidth =
    width > mediumscreenwidth
