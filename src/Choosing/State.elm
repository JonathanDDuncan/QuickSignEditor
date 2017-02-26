module Choosing.State exposing (init, update, subscriptions)

import Choosing.Types exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.EditorSign as EditorSign exposing (..)


init : String -> Int -> Int -> ( Choosing.Types.Model, Cmd Choosing.Types.Msg )
init key x y =
    ( { displaySign = EditorSign.signinit
      , valuestoAdd = []
      , value = key
      , offset = Offset x y
      }
    , Cmd.none
    )


update : Choosing.Types.Msg -> Choosing.Types.Model -> ( Choosing.Types.Model, Cmd Choosing.Types.Msg )
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


subscriptions : Choosing.Types.Model -> Sub Choosing.Types.Msg
subscriptions _ =
    Sub.none



-- To nest subscriptions
-- Sub.batch
--       [ SubChoosing.State.subscriptions model.subfeatureFieldName |> Sub.map SubChoosingMsg
--       ]
