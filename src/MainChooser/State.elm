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
      , selectedcolumn = 1
      , handgroupchoosings = []
      , groupselected = 256
      } 
      -- To initiate MainChooser state
      --  { MainChooserFieldName = fst MainChooser.State.init
      --  }
    , Cmd.batch [ Ports.requestInitialChoosings "", Ports.requestInitialGroupHandChoosings "" ]
    )


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

        ReceiveInitialChoosings choosings1 ->
            ( { model
                | choosings = List.map (toModel 0) choosings1
              }
            , Cmd.none
            )

        ReceiveInitialGroupHandChoosings chooserclassification ->
            let
                handgroupchoosings1 =
                    handgroupchoosings chooserclassification
            in
                ( { model
                    | handgroupchoosings = handgroupchoosings1
                  }
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

        GroupSelected base ->
            ( { model
                | groupselected = base
              }
            , Cmd.none
            )

        DragSymbol code ->
            ( model
            , cmdDragSymbol code
            )

handgroupchoosings chooserclassification =
    let
        itemsvalues =
            List.filter (\item -> item.choosertype == "handgroupchooser") chooserclassification.chooseritemvalues

        basechooseritems =
            List.filter (\item -> item.groupchooser == "handgroupchooser") chooserclassification.basechooseritems

        chooservalue =
            getchooservalue "handgroupchooser" itemsvalues

        handgroupchoosings =
            List.map (\item -> creategroupchoosing chooservalue itemsvalues item) basechooseritems
    in
         handgroupchoosings

getchooservalue
    : String
    -> List { a | choosertype : String, name : String, value : number }
    -> number
getchooservalue choosername itemsvalues =
    default choosername .value <|
        List.head <|
            List.filter (\item -> (item.choosertype == "groupchooser") && (item.name == choosername)) itemsvalues

default : String -> (a -> number) -> Maybe a -> number
default text func val =
    case val of
        Just n ->
            func n

        Nothing ->
            Debug.log (text ++ " not found") 0


creategroupchoosing chooservalue itemsvalues item =
    { base = item.base
    , name = item.name
    , symbolid = item.symbolid
    , symbolkey = item.symbolkey
    , unicodepua = item.unicodepua
    , validfills = item.validfills
    , validrotations = item.validrotations
    , groupchooser = chooservalue
    , common = item.common
    , subgroup1 = getvalue item.subgroup1 itemsvalues
    , subgroup2 = getvalue item.subgroup2 itemsvalues
    , rank = item.rank
    }

getvalue : String -> List { a | name : String, value : Int } -> Int
getvalue name itemsvalues =
    default name .value <|
        List.head <|
            List.filter (\item -> item.name == name) itemsvalues



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
