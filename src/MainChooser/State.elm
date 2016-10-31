module MainChooser.State exposing (init, update, subscriptions)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import MainChooser.Types exposing (..)
import Ports exposing (..)
import Choosing.State exposing (..)
import Choosing.Types exposing (..)


-- import SubMainChoosers.State


init : ( MainChooser.Types.Model, Cmd MainChooser.Types.Msg )
init =
    ( { choosings = [ fst (Choosing.State.init "S5" 6 8) ]
      , clicked = ""
      , selectedcolumn =
            1
            --   , handgroupchoosings = handgroupchoosingsinit
      }
      -- To initiate MainChooser state
      --  { MainChooserFieldName = fst MainChooser.State.init
      --  }
    , Cmd.batch [ Ports.requestInitialChoosings "", Ports.requestInitialGroupHandChoosings "" ]
    )



-- handgroupchoosingsinit =
--     { fistbabycommon = List.map (Choosing.Types.toModel 0) []
--     , fistringcommon = List.map (Choosing.Types.toModel 0) []
--     , fistmiddlecommon = List.map (Choosing.Types.toModel 0) []
--     , fistindexcommon = List.map (Choosing.Types.toModel 0) []
--     , fistthumbcommon = List.map (Choosing.Types.toModel 0) []
--     , circlethumbcommon = List.map (Choosing.Types.toModel 0) []
--     , circleindexcommon = List.map (Choosing.Types.toModel 0) []
--     , circleringcommon = List.map (Choosing.Types.toModel 0) []
--     , circlebabycommon = List.map (Choosing.Types.toModel 0) []
--     , cupbabycommon = List.map (Choosing.Types.toModel 0) []
--     , cupthumbcommon = List.map (Choosing.Types.toModel 0) []
--     , cupindexcommon = List.map (Choosing.Types.toModel 0) []
--     , anglethumbcommon = List.map (Choosing.Types.toModel 0) []
--     , anglebabycommon = List.map (Choosing.Types.toModel 0) []
--     , flatthumbcommon = List.map (Choosing.Types.toModel 0) []
--     , flatbabycommon = List.map (Choosing.Types.toModel 0) []
--     }


update : MainChooser.Types.Msg -> MainChooser.Types.Model -> ( MainChooser.Types.Model, Cmd MainChooser.Types.Msg )
update action model =
    case action of
        MainChooserMessage ->
            ( model
            , Cmd.none
            )

        Choosing msg ->
            ( model
            , Cmd.none
            )

        RequestInitialChoosings ->
            ( model
            , Ports.requestInitialChoosings ""
            )

        ReceiveInitialChoosings choosings ->
            let
                choosings1 =
                    MainChooser.Types.Model (List.map (Choosing.Types.toModel 0) choosings) "" 1
            in
                ( choosings1
                , Cmd.none
                )
 
        ReceiveInitialGroupHandChoosings chooserclassification ->
            let
                handgroupchoosings =
                  handgroupchoosings chooserclassification
            in
                -- ( { model | handgroupchoosings = converted }, Cmd.none )
                ( model
                , Cmd.none
                )

        Clicked clickvalue ->
            let
                choosings1 =
                    clickvalue
            in
                ( { model
                    | clicked = clickvalue
                  }
                , Cmd.none
                )

        SymbolView msg ->
            ( model
            , Cmd.none
            )

        SignView msg ->
            ( model
            , Cmd.none
            )

        SelectedColumn column ->
            ( { model
                | selectedcolumn = column
              }
            , Cmd.none
            )

handgroupchoosings chooserclassification =
    let itemsvalues = List.filter (\item -> item.choosertype == "handgroupchooser") chooserclassification.chooseritemvalues
        basechooseritems = List.filter (\item -> item.groupchooser == "handgroupchooser") chooserclassification.basechooseritems
        handgroupchoosings =         
    in


--To nest update of MainChooser
--  MainChooserMsg action ->
--          lift .MainChooserFieldName (\m x -> { m | MainChooserFieldName = x })  MainChooserMsg MainChooser.State.update action model


subscriptions : MainChooser.Types.Model -> Sub MainChooser.Types.Msg
subscriptions model =
    Sub.batch
        [ receiveInitialChoosings ReceiveInitialChoosings
        , receiveInitialGroupHandChoosings ReceiveInitialGroupHandChoosings
        ]



-- receiveInitialChoosings
-- To nest subscriptions
-- Sub.batch
--       [ SubMainChooser.State.subscriptions model.subMainChooserFieldName |> Sub.map SubMainChooserMsg
--       ]
