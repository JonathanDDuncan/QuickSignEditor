module Testing exposing (..)

 
import Choosers.State
import Choosers.View


main : Program Never
main =
    Html.App.program
        { init = Choosers.State.init
        , update = Choosers.State.update
        , subscriptions = Choosers.State.subscriptions
        , view = Choosers.View.root
        }
