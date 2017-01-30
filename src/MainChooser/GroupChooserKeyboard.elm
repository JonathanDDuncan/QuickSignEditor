module MainChooser.GroupChooserKeyboard exposing (..)

import MainChooser.Types exposing (..)
import Html
import Keyboard.Shared exposing (KeyAction)
import MainChooser.GeneralGroupChooser exposing (..)
import SWEditor.Display exposing (signView, symbolaloneView)
import List.Extra exposing (..)
import Exts.List exposing (chunk)


creategroupchooserkeyboard : Model -> List (KeyAction Msg)
creategroupchooserkeyboard model =
    let
        basesymbol =
            String.slice 0 4 model.clicked
    in
        case basesymbol of
            "S14c" ->
                handgroupchooserkeyboard model

            _ ->
                generalgroupchooserkeyboard model


generalgroupchooserkeyboard : Model -> List (KeyAction Msg)
generalgroupchooserkeyboard model =
    let
        keyranges =
            [ (List.range 43 52), (List.range 30 40), (List.range 16 28), (List.range 1 13) ]

        generalgroupchooserdata =
            List.concat <| model.generalgroupchooserdata

        allcolvalues =
            List.map .col generalgroupchooserdata
                |> unique

        cols =
            List.map (getcol generalgroupchooserdata) allcolvalues

        displacedcols =
            displace cols

        stackedcolumns =
            stackcolumns 4 displacedcols

        totalpages =
            gettotalpages keyranges stackedcolumns

        requestedpage =
            3

        page =
            if requestedpage > totalpages then
                totalpages
            else
                requestedpage

        pagedata =
            getpagedata page keyranges stackedcolumns

        colkeyranges =
            getcolranges keyranges pagedata

        groupchooserwithkey =
            List.concat <|
                List.map (\( keyrange, cols ) -> List.Extra.zip keyrange cols) colkeyranges
    in
        List.map
            (\( key, item ) ->
                { test = { key = key, ctrl = False, shift = False, alt = False }
                , action = (GroupSelected item.chooseritem)
                , display =
                    { width =
                        item.symbol.width
                    , height =
                        item.symbol.height
                    , view = Html.map SignView (symbolaloneView item.symbol 5)
                    }
                }
            )
            (groupchooserwithkey)


displace : List (List a) -> List (List a)
displace cols =
    let
        colcount =
            List.length cols

        colstoInsert =
            case colcount of
                1 ->
                    1

                2 ->
                    1

                3 ->
                    0

                _ ->
                    0

        toAdd =
            (List.range 1 colstoInsert)
                |> List.map (\i -> [])
                |> Debug.log "toAdd"

        newcols =
            List.append toAdd cols
                |> Debug.log "newcols"
    in
        newcols


getpagedata page keyranges stackedcolumns =
    let
        zipped =
            List.Extra.zip keyranges stackedcolumns

        pagedata =
            List.map
                (\( kr, sc ) ->
                    let
                        keyrangelength =
                            List.length kr

                        toskip =
                            (page - 1) * keyrangelength

                        data =
                            List.take keyrangelength <| List.drop (toskip) sc
                    in
                        data
                )
                zipped
    in
        pagedata


gettotalpages : List (List a) -> List (List a1) -> Int
gettotalpages keyranges stackedcolumns =
    let
        zipped =
            List.Extra.zip keyranges stackedcolumns

        pagespercol =
            List.map (\( kr, sc ) -> ceiling (toFloat (List.length sc) / toFloat (List.length kr))) zipped

        maxpage =
            List.maximum pagespercol
    in
        Maybe.withDefault 0 maxpage


getcolranges : List a -> List b -> List ( a, b )
getcolranges keyranges cols =
    List.Extra.zip keyranges cols


getcol : List { c | col : a, symboldatalist : List b } -> a -> List b
getcol combined col =
    List.concatMap (\dl -> dl.symboldatalist) <| List.filter (\item -> item.col == col) combined


stackcolumns : Int -> List (List b) -> List (List b)
stackcolumns maxcols cols =
    let
        tobechunked =
            completemissingcolumns maxcols cols

        chunked =
            chunk maxcols tobechunked

        folded =
            List.Extra.foldl1 appendcols chunked
    in
        case folded of
            Just value ->
                value

            Nothing ->
                []


completemissingcolumns : Int -> List (List b) -> List (List b)
completemissingcolumns maxcols cols =
    let
        grouping =
            toFloat (List.length cols) / toFloat maxcols |> ceiling

        totalcolsneeded =
            grouping * maxcols

        colsmissing =
            totalcolsneeded - List.length cols

        tobechunked =
            List.append cols <| List.map (\i -> []) (List.range 1 colsmissing)
    in
        tobechunked


appendcols : List (List a) -> List (List a) -> List (List a)
appendcols list1 list2 =
    let
        zipped =
            List.Extra.zip list1 list2

        appendedlists =
            List.map (\( a, b ) -> List.append a b) zipped
    in
        appendedlists


handgroupchooserkeyboard model =
    []
