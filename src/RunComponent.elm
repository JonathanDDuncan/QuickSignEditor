module RunComponent exposing (..)

import Choosers.State
import Choosers.View
import Choosers.Types
import Html
import Html


main : Program Never Choosers.Types.Model Choosers.Types.Msg
main =
    let
        height =
            50
    in
        Html.program
            { init = Choosers.State.init
            , update = Choosers.State.update
            , subscriptions = Choosers.State.subscriptions
            , view = Choosers.View.choosingroot height
            }
