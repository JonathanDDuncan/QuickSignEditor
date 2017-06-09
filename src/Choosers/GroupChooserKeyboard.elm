module Choosers.GroupChooserKeyboard exposing (creategroupchooserkeyboard, totalkeyboardpages)

import Choosers.Types
    exposing
        ( Model
        , Msg(EditorMsg, KeyboardMsg)
        , Editor(GroupSelected)
        , KeyboardType(NextKeyboardPage)
        , ishandgroupchooser
        )
import Html
import Keyboard.Shared exposing (KeyAction)
import SW.Display exposing (symbolsvg)
import SW.Symbol exposing (Symbol)
import List.Extra exposing (unique)
import Exts.List exposing (chunk)
import Choosers.HandGroupChooser exposing (createhandgroupchooserdata)
import Choosers.GeneralGroupChooser exposing (creategeneralgroupchooserdata)


creategroupchooserkeyboard : Model -> List (KeyAction Msg)
creategroupchooserkeyboard model =
    if ishandgroupchooser model.clicked then
        handgroupchooserkeyboard model
    else
        generalgroupchooserkeyboard model


totalkeyboardpages : Model -> Int
totalkeyboardpages model =
    (if ishandgroupchooser model.clicked then
        getprepagedatahand <| createhandgroupchooserdata model
     else
        getprepagedatageneral <| creategeneralgroupchooserdata model
    )
        |> gettotalpages keyranges


generalgroupchooserkeyboard : Model -> List (KeyAction Msg)
generalgroupchooserkeyboard model =
    model
        |> creategeneralgroupchooserdata
        |> getprepagedatageneral
        |> pagedata model.chooserskeyboard.keyboardpage
        |> createkeyactionlist


pagedata : Int -> List (List a1) -> List (List a1)
pagedata page prepagedata =
    let
        totalpages =
            gettotalpages keyranges prepagedata

        currentpage =
            if page > totalpages then
                totalpages
            else
                page
    in
        getpagedata currentpage keyranges prepagedata


createkeyactionlist :
    List
        (List
            { a
                | chooseritem : Choosers.Types.ChooserItem
                , symbol : Symbol
            }
        )
    -> List (KeyAction Msg)
createkeyactionlist data =
    data
        |> getcolranges keyranges
        |> List.map (\( keyrange, cols ) -> List.Extra.zip keyrange cols)
        |> List.concat
        |> List.map
            (\( key, item ) ->
                { test = { key = key, ctrl = False, shift = False, alt = False }
                , action = (EditorMsg << GroupSelected) item.chooseritem
                , display =
                    { width = item.symbol.width
                    , height = item.symbol.height
                    , backgroundcolor = Nothing
                    , view = symbolsvg "" item.symbol
                    }
                }
            )
        |> List.append nextpagelist


getprepagedatageneral :
    List (List { c | symboldatalist : List b, col : comparable })
    -> List (List b)
getprepagedatageneral generalgroupchooserdata =
    let
        concatenated =
            generalgroupchooserdata |> List.concat
    in
        List.map .col concatenated
            |> unique
            |> List.map (getcolgeneral concatenated)
            |> displace
            |> stackcolumns 4


keyranges : List (List Int)
keyranges =
    [ List.range 43 52, List.range 30 40, List.range 16 28, List.range 1 13 ]


nextpagelist : List (KeyAction Msg)
nextpagelist =
    [ { test = { key = 57, ctrl = False, shift = False, alt = False }
      , action = KeyboardMsg <| NextKeyboardPage
      , display =
            { width =
                50
            , height =
                30
            , backgroundcolor = Nothing
            , view = Html.text "Next Page"
            }
      }
    ]


displace : List (List a) -> List (List a)
displace cols =
    let
        colstoInsert =
            case List.length cols of
                1 ->
                    1

                2 ->
                    1

                3 ->
                    0

                _ ->
                    0
    in
        List.range 1 colstoInsert
            |> List.map (\_ -> [])
            |> List.append cols


getpagedata : Int -> List (List a) -> List (List a1) -> List (List a1)
getpagedata page keyranges stackedcolumns =
    List.map
        (\( kr, sc ) ->
            List.drop ((page - 1) * List.length kr) sc
                |> List.take (List.length kr)
        )
        (List.Extra.zip keyranges stackedcolumns)


gettotalpages : List (List a) -> List (List a1) -> Int
gettotalpages keyranges stackedcolumns =
    List.Extra.zip keyranges stackedcolumns
        |> List.map (\( kr, sc ) -> ceiling (toFloat (List.length sc) / toFloat (List.length kr)))
        |> List.maximum
        |> Maybe.withDefault 0


getcolranges : List a -> List b -> List ( a, b )
getcolranges keyranges cols =
    List.Extra.zip keyranges cols


getcolgeneral : List { c | col : a, symboldatalist : List b } -> a -> List b
getcolgeneral combined col =
    List.concatMap (\dl -> dl.symboldatalist) <| List.filter (\item -> item.col == col) combined


stackcolumns : Int -> List (List b) -> List (List b)
stackcolumns maxcols cols =
    completemissingcolumns maxcols cols
        |> chunk maxcols
        |> List.Extra.foldl1 appendcols
        |> Maybe.withDefault []


completemissingcolumns : Int -> List (List b) -> List (List b)
completemissingcolumns maxcols cols =
    (toFloat (List.length cols) / toFloat maxcols)
        |> ceiling
        |> (\grouping -> maxcols * grouping)
        |> (\totalcolsneeded -> totalcolsneeded - List.length cols)
        |> List.range 1
        |> List.map (\_ -> [])
        |> List.append cols


appendcols : List (List a) -> List (List a) -> List (List a)
appendcols list1 list2 =
    List.Extra.zip list1 list2
        |> List.map (\( a, b ) -> List.append a b)


handgroupchooserkeyboard : Model -> List (KeyAction Msg)
handgroupchooserkeyboard model =
    model
        |> createhandgroupchooserdata
        |> getprepagedatahand
        |> pagedata model.chooserskeyboard.keyboardpage
        |> createkeyactionlist


getprepagedatahand : List (List { a | col : Int, symboldatalist : List b }) -> List (List b)
getprepagedatahand handgroupchooserdata =
    let
        concatenated =
            handgroupchooserdata
                |> List.concat
    in
        [ gethandcolumnvalues concatenated 0
        , gethandcolumnvalues concatenated 1
        , List.append (gethandcolumnvalues concatenated 2) (gethandcolumnvalues concatenated 3)
        , gethandcolumnvalues concatenated 4
        ]


gethandcolumnvalues : List { c | col : Int, symboldatalist : List b } -> Int -> List b
gethandcolumnvalues values col =
    values
        |> List.filter (\item -> item.col == col)
        |> List.concatMap (\dl -> dl.symboldatalist)
