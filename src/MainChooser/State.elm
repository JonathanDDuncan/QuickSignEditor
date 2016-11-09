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
import Exts.List exposing (..)


-- import SubMainChoosers.State


init : ( MainChooser.Types.Model, Cmd MainChooser.Types.Msg )
init =
    ( { choosings = [ fst (Choosing.State.init "S5" 6 8) ]
      , clicked = ""
      , selectedcolumn = 1
      , handgroupchoosings = []
      , allgroupchoosings =
            [ { basesymbol = ""
              , choosings = []
              }
            ]
      , groupselected = chooseriteminit
      , handgroupfilter = 1
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
                allgroupchoosings1 =
                    allgroupchoosings chooserclassification
            in
                ( { model
                    | allgroupchoosings = allgroupchoosings1
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

        GroupSelected choosing ->
            ( { model
                | groupselected = Debug.log "GroupSelected choosing" choosing
              }
            , Cmd.none
            )

        DragSymbol code ->
            ( model
            , cmdDragSymbol <| code
            )

        FilterHandGroup value ->
            ( { model
                | handgroupfilter = value
              }
            , Cmd.none
            )


allgroupchoosings chooserclassification =
    let
        basesymbols =
            List.sort <| unique <| List.filter (\value -> value /= "") <| List.map (\item -> item.symbolgroup) chooserclassification.chooseritemvalues

        allgroupchoosings1 =
            List.map (\basesymbol1 -> { basesymbol = basesymbol1, choosings = getchoosings basesymbol1 chooserclassification.chooseritemvalues chooserclassification.basechooseritems }) basesymbols
    in
        allgroupchoosings1


getchoosings symbolgroup chooseritemvalues basechooseritems =
    let
        groupchoosers =
            List.sort <| unique <| List.map (\item -> item.name) <| List.filter (\item -> item.choosertype == "groupchooser" && item.symbolgroup == symbolgroup) chooseritemvalues

        items =
            List.filter (\basechooseritem -> List.any (is basechooseritem.groupchooser) groupchoosers) basechooseritems

        itemsvalues =
            List.filter (\chooseritemvalue -> List.any (is chooseritemvalue.choosertype) groupchoosers) chooseritemvalues

        planeitemsvalues =
            List.filter (\chooseritemvalue -> chooseritemvalue.choosertype == "plane") chooseritemvalues

        converted =
            List.map (\item -> creategroupchoosing (getchooservalue item.groupchooser chooseritemvalues) itemsvalues planeitemsvalues item) items
    in
        converted


is str1 str2 =
    str1 == str2


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
            if text /= "" then
                Debug.log (text ++ " not found") 0
            else
                0


creategroupchoosing chooservalue itemsvalues planeitemsvalues item =
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
    , plane = getvalue item.plane planeitemsvalues
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
