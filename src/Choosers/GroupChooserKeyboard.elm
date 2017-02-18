module Choosers.GroupChooserKeyboard exposing (..)

import Choosers.Types exposing (..)
import Html
import Keyboard.Shared exposing (KeyAction)
import SWEditor.DisplaySvg exposing (symbolsvg)
import List.Extra exposing (..)
import Exts.List exposing (chunk)
import Choosers.HandGroupChooser exposing (createhandgroupchooserdata)


creategroupchooserkeyboard : Model -> List (KeyAction Msg)
creategroupchooserkeyboard model =
    if ishandgroupchooser model.clicked then
        handgroupchooserkeyboard model
    else
        generalgroupchooserkeyboard model


ishandgroupchooser : String -> Bool
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


pagedata : List (List a1) -> Int -> List (List a1)
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
                        , view = Html.map SignView (symbolsvg item.symbol)
                        }
                    }
                )
                (groupchooserwithkey)

        listwithnextpage =
            List.append viewkeylist nextpagelist
    in
        listwithnextpage


getprepagedatageneral :
    List (List { c | symboldatalist : List b, col : comparable })
    -> List (List b)
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


keyranges : List (List Int)
keyranges =
    [ (List.range 43 52), (List.range 30 40), (List.range 16 28), (List.range 1 13) ]


nextpagelist :
    List
        { action : Msg
        , display : { height : number, view : Html.Html msg, width : number1 }
        , test : { alt : Bool, ctrl : Bool, key : Int, shift : Bool }
        }
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


getpagedata : Int -> List (List a) -> List (List a1) -> List (List a1)
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


handgroupchooserkeyboard : Model -> List (KeyAction Msg)
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
        keyactionlist


getprepagedatahand : List (List { a | col : Int, symboldatalist : List b }) -> List (List b)
getprepagedatahand handgroupchooserdata =
    let
        concatenated =
            List.concat <|
                handgroupchooserdata

        col1 =
            gethandcolumnvalues concatenated 0

        col2 =
            gethandcolumnvalues concatenated 1

        col3 =
            gethandcolumnvalues concatenated 2

        col4 =
            gethandcolumnvalues concatenated 3

        col5 =
            gethandcolumnvalues concatenated 4

        cols =
            [ col1, col2, List.append col3 col4, col5 ]
    in
        cols


gethandcolumnvalues : List { c | col : Int, symboldatalist : List b } -> Int -> List b
gethandcolumnvalues values col =
    List.concatMap (\dl -> dl.symboldatalist) <| List.filter (\item -> item.col == col) values
