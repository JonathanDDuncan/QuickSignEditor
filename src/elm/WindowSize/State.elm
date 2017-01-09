module WindowSize.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import WindowSize.Types exposing (..)
import Task
import Window


-- import SubWindows.State


init : ( Model, Cmd Msg )
init =
    ( { windowSize =
            Window.Size 1200 600
      }
    , Cmd.none
      -- , Task.perform (\_ -> Idle) (\x -> Resize x) Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( { model
                | windowSize = newSize
              }
            , Cmd.none
            )

        Idle ->
            ( model, Cmd.none )



--To nest update of Window
--  WindowMsg action ->
--          lift .WindowFieldName (\m x -> { m | WindowFieldName = x })  WindowMsg Window.State.update action model


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize



-- To nest subscriptions
-- Sub.batch
--       [ SubWindow.State.subscriptions model.subWindowFieldName |> Sub.map SubWindowMsg
--       ]
