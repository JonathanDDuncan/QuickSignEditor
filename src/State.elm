module State exposing (init, update, subscriptions)

import Types exposing (Model, Msg(..))
import PlatformHelpers exposing (lift)
import Overlay.State


init : ( Types.Model, Cmd Types.Msg )
init =
    ( { overlay = Tuple.first Overlay.State.init
      }
    , Cmd.map Overlay (Tuple.second Overlay.State.init)
    )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Overlay.State.subscriptions model.overlay |> Sub.map Overlay
        ]


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update action model =
    case action of
        Overlay action ->
            lift .overlay (\m x -> { m | overlay = x }) Overlay Overlay.State.update action model
