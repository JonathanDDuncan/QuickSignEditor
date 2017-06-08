module WindowSize.State exposing (init, update, subscriptions)

import WindowSize.Types exposing (Model, Msg(..))
import Task
import Window


init : ( Model, Cmd Msg )
init =
    ( { windowSize =
            Window.Size 1200 600
      }
    , initialSizeCmd
    )


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform Resize Window.size


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


subscriptions : Sub Msg
subscriptions =
    Window.resizes Resize
