module Drawer.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import Drawer.Types exposing (..)
import Material


-- import SubDrawers.State


init : ( Drawer.Types.Model, Cmd Drawer.Types.Msg )
init =
    ( { side = "right"
      , active = True
      , showing = True
      , alwaysShowpx = 20
      , fullwidth = 200
      , height = 800
      , mdl = Material.model
      }
      -- To initiate Drawer state
      --  { DrawerFieldName = fst Drawer.State.init
      --  }
    , Cmd.none
    )


update : Drawer.Types.Msg -> Drawer.Types.Model -> ( Drawer.Types.Model, Cmd Drawer.Types.Msg )
update action model =
    case action of
        Show ->
            ( { model | showing = True }
            , Cmd.none
            )

        Hide ->
            ( { model | showing = False }
            , Cmd.none
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg' ->
            Material.update msg' model



--To nest update of Drawer
--  DrawerMsg action ->
--          lift .DrawerFieldName (\m x -> { m | DrawerFieldName = x })  DrawerMsg Drawer.State.update action model


subscriptions : Drawer.Types.Model -> Sub Drawer.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubDrawer.State.subscriptions model.subDrawerFieldName |> Sub.map SubDrawerMsg
--       ]
