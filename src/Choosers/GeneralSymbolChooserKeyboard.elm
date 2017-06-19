module Choosers.GeneralSymbolChooserKeyboard exposing (createsymbolchooserkeyboard)

import Html exposing (Html)
import Choosers.Types exposing (Model, Msg(EditorMsg, SelectHand, SelectPlane, SelectHandFill))
import Choosers.EditorType as Editor exposing (Editor)
import Keyboard.Shared exposing (KeyAction)
import Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser, reorderedcolumnforpetal2)
import List.Extra
import SW.Display exposing (signdisplaysvg)
import Choosers.HandSymbolChooser exposing (wallplaneimg, floorplaneimg)
import Helpers.MaybeExtra exposing (removeNothings)
import SW.Sign exposing (signinit)
import SW.Symbol exposing (Symbol)
import SW.Pua exposing (ishand)
import SW.HandsType as Hands exposing (Hands(..))
import SW.HandFillsType exposing (HandFills(..))
import SW.PlanesType as Planes exposing (Planes(..))


createsymbolchooserkeyboard : Model -> List (KeyAction Msg)
createsymbolchooserkeyboard model =
    if ishand model.clicked then
        createhandsymbolchooserkeyboard model
    else
        creategeneralsymbolchooserkeyboard model


handflowerkeyidrange : List Int
handflowerkeyidrange =
    [ 24, 23, 36, 49, 50, 51, 39, 25 ]


flower1keyidrange : List Int
flower1keyidrange =
    [ 21, 20, 33, 46, 47, 48, 36, 22 ]


flower2keyidrange : List Int
flower2keyidrange =
    [ 25, 24, 37, 50, 51, 52, 40, 26 ]


creategeneralsymbolchooserkeyboard : Model -> List (KeyAction Msg)
creategeneralsymbolchooserkeyboard model =
    let
        { generalsymbolrowdata, symbolcolumnsdata } =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn

        firstcolumn =
            getgeneralsymbolcolumn symbolcolumnsdata.column1

        secondcolumn =
            getgeneralsymbolcolumn symbolcolumnsdata.column2
    in
        List.concat
            [ createkeyactionlist generalsymbolrowdata (List.range 16 19)
            , createflowerkeyactionlist firstcolumn
                flower1keyidrange
                { key = 0, ctrl = False, shift = False, alt = False }
                (EditorMsg << Editor.AddSymbol)
                "green"
                |> setassamesize
            , createflowerkeyactionlist (reorderedcolumnforpetal2 secondcolumn)
                flower2keyidrange
                { key = 0, ctrl = False, shift = False, alt = False }
                (EditorMsg << Editor.AddSymbol)
                "green"
                |> setassamesize
            , createflowerkeyactionlist firstcolumn
                flower1keyidrange
                { key = 0, ctrl = False, shift = True, alt = False }
                (EditorMsg << Editor.ReplaceSymbol)
                "red"
                |> setassamesize
            , createflowerkeyactionlist (reorderedcolumnforpetal2 secondcolumn)
                flower2keyidrange
                { key = 0, ctrl = False, shift = True, alt = False }
                (EditorMsg << Editor.ReplaceSymbol)
                "red"
                |> setassamesize
            ]


setassamesize :
    List { b | display : { a | height : comparable, width : comparable } }
    -> List { b | display : { a | height : comparable, width : comparable } }
setassamesize flowerkeyactionlist =
    List.map
        (\fka ->
            let
                previousdisplay =
                    fka.display
            in
                { fka
                    | display =
                        { previousdisplay
                            | height =
                                List.map (\fka1 -> fka1.display.height) flowerkeyactionlist
                                    |> List.maximum
                                    |> Maybe.withDefault 0
                            , width =
                                List.map (\fka2 -> fka2.display.width) flowerkeyactionlist
                                    |> List.maximum
                                    |> Maybe.withDefault 0
                        }
                }
        )
        flowerkeyactionlist


getgeneralsymbolcolumn : List (Maybe a) -> List a
getgeneralsymbolcolumn symbolcolumndata =
    symbolcolumndata
        |> removeNothings


createkeyactionlist : List { fill : Int, symbol : Symbol } -> List Int -> List (KeyAction Msg)
createkeyactionlist data range =
    List.Extra.zip range data
        |> List.map
            (\( key, item ) ->
                { test = { key = key, ctrl = False, shift = False, alt = False }
                , action = (Choosers.Types.EditorMsg << Editor.SelectedColumn) item.fill
                , display =
                    { width =
                        item.symbol.width
                    , height =
                        item.symbol.height
                    , backgroundcolor = Nothing
                    , view =
                        signdisplaysvg "" { signinit | syms = [ item.symbol ] }
                    }
                }
            )


createhandsymbolchooserkeyboard : Model -> List (KeyAction Msg)
createhandsymbolchooserkeyboard model =
    let
        petals =
            List.map (\petal -> petal.symbol) model.handsymbol.flowersymbols
    in
        List.concat
            [ createhandkeyactionlist (SelectHand Hands.Left) [ model.handsymbol.symbollefthand ] [ 16 ]
            , createhandkeyactionlist (SelectHand Hands.Right) [ model.handsymbol.symbolrighthand ] [ 17 ]
            , [ createplanekeyaction (SelectPlane Planes.Wall) wallplaneimg 50 20 18 ]
            , [ createplanekeyaction (SelectPlane Planes.Floor) floorplaneimg 50 20 19 ]
            , createfillkeyactionlist (List.reverse model.handsymbol.handfillitems) (List.range 30 33)
            , createflowerkeyactionlist petals
                handflowerkeyidrange
                { key = 0, ctrl = False, shift = False, alt = False }
                (EditorMsg << Editor.AddSymbol)
                "green"
                |> setassamesize
            , createflowerkeyactionlist petals
                handflowerkeyidrange
                { key = 0, ctrl = False, shift = True, alt = False }
                (EditorMsg << Editor.ReplaceSymbol)
                "red"
                |> setassamesize
            ]


createfillkeyactionlist :
    List
        { b
            | filltype : HandFills
            , symbol :
                { height : Int
                , id : Int
                , key : String
                , nbcolor : String
                , nwcolor : String
                , selected : Bool
                , size : Float
                , width : Int
                , x : Int
                , y : Int
                }
        }
    -> List a
    -> List
        { action : Msg
        , display :
            { backgroundcolor : Maybe a1
            , height : Int
            , view : Html Msg
            , width : Int
            }
        , test : { alt : Bool, ctrl : Bool, key : a, shift : Bool }
        }
createfillkeyactionlist data range =
    List.Extra.zip range data
        |> List.map
            (\( key, handfillitem ) ->
                { test = { key = key, ctrl = False, shift = False, alt = False }
                , action = SelectHandFill handfillitem.filltype
                , display =
                    { width =
                        handfillitem.symbol.width
                    , height =
                        handfillitem.symbol.height
                    , backgroundcolor = Nothing
                    , view =
                        signdisplaysvg "" { signinit | syms = [ handfillitem.symbol ] }
                    }
                }
            )


createflowerkeyactionlist :
    List Symbol
    -> List a
    -> { c | key : b }
    -> (String -> d)
    -> e
    -> List
        { action : d
        , display :
            { backgroundcolor : Maybe e
            , height : Int
            , view : Html Msg
            , width : Int
            }
        , test : { c | key : a }
        }
createflowerkeyactionlist data range modifiers action backgroundcolor =
    List.Extra.zip range data
        |> List.map
            (\( key, symbol ) ->
                { test = { modifiers | key = key }
                , action = action symbol.key
                , display =
                    { width =
                        symbol.width
                    , height =
                        symbol.height
                    , backgroundcolor = Just backgroundcolor
                    , view =
                        signdisplaysvg "" { signinit | syms = [ symbol ] }
                    }
                }
            )


createhandkeyactionlist :
    b
    -> List Symbol
    -> List a
    -> List
        { action : b
        , display :
            { backgroundcolor : Maybe a1
            , height : Int
            , view : Html Msg
            , width : Int
            }
        , test : { alt : Bool, ctrl : Bool, key : a, shift : Bool }
        }
createhandkeyactionlist message data range =
    List.Extra.zip range data
        |> List.map
            (\( key, symbol ) ->
                { test = { key = key, ctrl = False, shift = False, alt = False }
                , action = message
                , display =
                    { width =
                        symbol.width
                    , height =
                        symbol.height
                    , backgroundcolor = Nothing
                    , view =
                        signdisplaysvg "" { signinit | syms = [ symbol ] }
                    }
                }
            )


createplanekeyaction :
    b
    -> c
    -> d
    -> e
    -> f
    -> { action : b
       , display : { backgroundcolor : Maybe a, height : e, view : c, width : d }
       , test : { alt : Bool, ctrl : Bool, key : f, shift : Bool }
       }
createplanekeyaction message view width height key =
    { test = { key = key, ctrl = False, shift = False, alt = False }
    , action = message
    , display =
        { width =
            width
        , height =
            height
        , backgroundcolor = Nothing
        , view = view
        }
    }
