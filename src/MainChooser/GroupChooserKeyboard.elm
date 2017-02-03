module MainChooser.GroupChooserKeyboard exposing (..)

import MainChooser.Types exposing (..)
import Html
import Keyboard.Shared exposing (KeyAction)


-- import MainChooser.GeneralGroupChooser exposing (..)

import SWEditor.Display exposing (signView, symbolaloneView)
import List.Extra exposing (..)
import Exts.List exposing (chunk)
import MainChooser.HandGroupChooser exposing (createhandgroupchooserdata)


creategroupchooserkeyboard : Model -> List (KeyAction Msg)
creategroupchooserkeyboard model =
    if ishandgroupchooser model.clicked then
        handgroupchooserkeyboard model
    else
        generalgroupchooserkeyboard model


ishandgroupchooser clicked =
    let
        basesymbol =
            String.slice 0 4 clicked
    in
        case basesymbol of
            "S14c" ->
                True

            _ ->
                False


totalkeyboardpages : Model -> Int
totalkeyboardpages model =
    let
        prepagedata =
            if ishandgroupchooser model.clicked then
                getprepagedatahand <| createhandgroupchooserdata model
            else
                getprepagedatageneral model.generalgroupchooserdata

        totalpages =
            gettotalpages keyranges prepagedata
    in
        totalpages


generalgroupchooserkeyboard : Model -> List (KeyAction Msg)
generalgroupchooserkeyboard model =
    let
        prepagedata =
            getprepagedatageneral model.generalgroupchooserdata

        pageddata =
            pagedata prepagedata model.chooserskeyboard.keyboardpage

        keyactionlist =
            createkeyactionlist pageddata
    in
        keyactionlist


pagedata prepagedata page =
    let
        totalpages =
            gettotalpages keyranges prepagedata

        currentpage =
            if page > totalpages then
                totalpages
            else
                page

        pagedata =
            getpagedata currentpage keyranges prepagedata
    in
        pagedata


createkeyactionlist data =
    let
        colkeyranges =
            getcolranges keyranges data

        groupchooserwithkey =
            List.concat <|
                List.map (\( keyrange, cols ) -> List.Extra.zip keyrange cols) colkeyranges

        viewkeylist =
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

        listwithnextpage =
            List.append viewkeylist nextpagelist
    in
        listwithnextpage


getprepagedatageneral generalgroupchooserdata =
    let
        concatenated =
            List.concat <| generalgroupchooserdata

        allcolvalues =
            List.map .col concatenated
                |> unique

        cols =
            List.map (getcolgeneral concatenated) allcolvalues

        displacedcols =
            displace cols

        stackedcolumns =
            stackcolumns 4 displacedcols
    in
        stackedcolumns


keyranges =
    [ (List.range 43 52), (List.range 30 40), (List.range 16 28), (List.range 1 13) ]


nextpagelist =
    [ { test = { key = 57, ctrl = False, shift = False, alt = False }
      , action = NextKeyboardPage
      , display =
            { width =
                50
            , height =
                30
            , view = Html.text "Next Page"
            }
      }
    ]


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

        newcols =
            List.append toAdd cols
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


getcolgeneral : List { c | col : a, symboldatalist : List b } -> a -> List b
getcolgeneral combined col =
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
    let
        handgroupchooserdata =
            createhandgroupchooserdata model

        prepagedatahand =
            getprepagedatahand handgroupchooserdata

        pageddata =
            pagedata prepagedatahand model.chooserskeyboard.keyboardpage

        keyactionlist =
            createkeyactionlist pageddata
    in
        []


getprepagedatahand handgroupchooserdata =
    let
        data =
            handgroupchooserdata

        allcolvalues =
            List.range 1 5

        cols =
            List.map getcolhand data

        -- stackedcolumns =
        --     stackcolumns 4 displacedcols
    in
        cols


getcolhand data =
    List.concatMap (\dl -> dl.symboldatalist) data
