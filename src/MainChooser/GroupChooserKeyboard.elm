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
        generalgroupchooserdata =
            List.concat <| creategeneralgroupchooserdata model

        allcolvalues =
            List.map .col generalgroupchooserdata
                |> unique

        cols =
            List.map (getcol generalgroupchooserdata) allcolvalues

        stackedcolumns =
            stackcolumns 4 cols allcolvalues

        keyranges =
            [ (List.range 43 52), (List.range 30 40), (List.range 16 28), (List.range 1 13) ]

        colkeyranges =
            getcolranges keyranges stackedcolumns

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


getcolranges : List a -> List b -> List ( a, b )
getcolranges keyranges cols =
    List.Extra.zip keyranges cols


getcol : List { c | col : a, symboldatalist : List b } -> a -> List b
getcol combined col =
    List.concatMap (\dl -> dl.symboldatalist) <| List.filter (\item -> item.col == col) combined


stackcolumns : Int -> List (List b) -> List a -> List (List b)
stackcolumns maxcols cols allcolvalues =
    let
        tobechunked =
            completemissingcolumns maxcols allcolvalues cols

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


completemissingcolumns : Int -> List a -> List (List b) -> List (List b)
completemissingcolumns maxcols colvalues cols =
    let
        grouping =
            toFloat (List.length colvalues) / toFloat maxcols |> ceiling

        totalcolsneeded =
            grouping * maxcols

        colsmissing =
            totalcolsneeded - List.length colvalues

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
