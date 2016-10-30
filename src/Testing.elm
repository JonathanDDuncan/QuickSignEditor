module Testing exposing (..)

import Html.App
import MainChooser.State
import MainChooser.View


main : Program Never
main =
    Html.App.program
        { init = MainChooser.State.init
        , update = MainChooser.State.update
        , subscriptions = MainChooser.State.subscriptions
        , view = MainChooser.View.root
        }
