module FSWTestHelper exposing (..)

import SW.FSW as FSW exposing (..)
import Dict
import SW.Types exposing (Sign, Size, signinit)
import SW.Symbol exposing (Symbol)
import SWEditor.EditorSymbol exposing (createSymbolbyKey)


worldsign : Sign
worldsign =
    { signinit | syms = [ symbol1, symbol2, symbol3, symbol4 ] }


symbol1 : Symbol
symbol1 =
    let
        symbol =
            createSymbolbyKey "S1870a" partialsymbolsizes
    in
        { symbol | x = 295, y = 289 }


symbol2 : Symbol
symbol2 =
    let
        symbol =
            createSymbolbyKey "S18701" partialsymbolsizes
    in
        { symbol | x = 288, y = 264 }


symbol3 : Symbol
symbol3 =
    let
        symbol =
            createSymbolbyKey "S20500" partialsymbolsizes
    in
        { symbol | x = 314, y = 270 }


symbol4 : Symbol
symbol4 =
    let
        symbol =
            createSymbolbyKey "S2e734" partialsymbolsizes
    in
        { symbol | x = 306, y = 242 }


getfield : (a -> String) -> Maybe a -> String
getfield getter recd =
    (\mayb ->
        case mayb of
            Nothing ->
                "Nothing"

            Just value ->
                getter value
    )
        recd


world : String
world =
    "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468"


worldspelling : String
worldspelling =
    "AS18701S1870aS2e734S20500" ++ world


defaultResultsign : String -> Sign
defaultResultsign fsw =
    Result.withDefault signinit <| FSW.fswtoSign partialsymbolsizes fsw


getsignerrmessage : String -> String
getsignerrmessage fsw =
    let
        result =
            FSW.fswtoSign partialsymbolsizes fsw
    in
        case result of
            Ok value ->
                "Ok"

            Err msg ->
                msg


worldbluegreen : String
worldbluegreen =
    world ++ "-D_blue,green_"


worldrgbredmagenta : String
worldrgbredmagenta =
    world ++ "-D_f44242,f441ee_"


worldthirdbluegreen : String
worldthirdbluegreen =
    world ++ "--D03_blue,green_"


worldsecondrgbredmagenta : String
worldsecondrgbredmagenta =
    world ++ "--D02_f44242,f441ee_"


worldsecondzoomdoubleadjusted : String
worldsecondzoomdoubleadjusted =
    world ++ "--Z02,2.3,480x490"


allsymbolscolor : (Symbol -> String) -> String -> Sign -> Bool
allsymbolscolor getter color sign =
    List.all (\symbol -> getter symbol == color) sign.syms



-- partialsymbolsizes only as place holder can be populated with some real values for specific cases


partialsymbolsizes : Dict.Dict String Size
partialsymbolsizes =
    let
        symbolsizes =
            [ { k = "S1870a", w = 29, h = 18 }
            , { k = "S18701", w = 24, h = 24 }
            , { k = "S20500", w = 10, h = 11 }
            , { k = "S2e734", w = 16, h = 25 }
            ]
    in
        Dict.fromList <|
            List.map (\symbolsize -> (.k symbolsize) => (Size (.w symbolsize) (.h symbolsize))) symbolsizes
