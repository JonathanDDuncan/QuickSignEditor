module Choosing.State exposing (init, update, subscriptions)

import Choosing.Types exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.EditorSign as EditorSign exposing (..)


init : String -> Int -> Int -> ( Choosing.Types.ChoosingModel, Cmd Choosing.Types.ChoosingMsg )
init key x y =
    ( { displaySign = EditorSign.signinit
      , valuestoAdd = []
      , value = key
      , offset = Offset x y
      }
    , Cmd.none
    )


update : Choosing.Types.ChoosingMsg -> Choosing.Types.ChoosingModel -> ( Choosing.Types.ChoosingModel, Cmd Choosing.Types.ChoosingMsg )
update action model =
    case action of
        ChoosingMessage ->
            ( model
            , Cmd.none
            )

        Display msg ->
            ( model
            , Cmd.none
            )



--To nest update of feature
--  ChoosingMsg action ->
--          lift .featureFieldName (\m x -> { m | featureFieldName = x })  ChoosingMsg Choosing.State.update action model


subscriptions : Choosing.Types.ChoosingModel -> Sub Choosing.Types.ChoosingMsg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubChoosing.State.subscriptions model.subfeatureFieldName |> Sub.map SubChoosingMsg
--       ]
