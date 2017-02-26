module FSWTests exposing (..)

import Test exposing (..)
import Expect
import SW.FSW as FSW exposing (..)
import SWEditor.EditorSign exposing (..)


fswTests : Test
fswTests =
    describe "FSW to EditorSign Test Suite"
        [ test "Empty sign x 500" <|
            \() ->
                Expect.equal (defaultResultsign "M500x500").x 500
        , test "Empty sign 500x500" <|
            \() ->
                Expect.equal (getsignerrmessage "500x500") "Could not find lane and coordinate from '500x500'."
        , test "Empty sign 000x500" <|
            \() ->
                Expect.equal (getsignerrmessage "000x500") "Could not find lane and coordinate from '000x500'."
        , test "Empty sign 500500" <|
            \() ->
                Expect.equal (getsignerrmessage "500500") "Could not get x coordinate from ''. | Could not get coordinate from '500500'."
        , test "Empty sign M500500" <|
            \() ->
                Expect.equal (getsignerrmessage "M500500") "Could not get x coordinate from ''. | Could not get coordinate from 'M500500'."
        , test "Empty sign M500f500" <|
            \() ->
                Expect.equal (getsignerrmessage "M500f500") "Could not get x coordinate from ''. | Could not get coordinate from 'M500f500'."
        , test "Empty sign " <|
            \() ->
                Expect.equal (getsignerrmessage "") "Could not get x coordinate from ''. | Could not get coordinate from ''."
        , test "Empty sign y 500" <|
            \() ->
                Expect.equal (defaultResultsign "M500x500").y 500
        , test "Empty sign x 498" <|
            \() ->
                Expect.equal (defaultResultsign "M498x497").x 498
        , test "Empty sign y 497" <|
            \() ->
                Expect.equal (defaultResultsign "M498x497").y 497
        , test "Empty sign no symbols" <|
            \() -> Expect.equal (defaultResultsign "M500x500").syms []
        , test "lane is middle lane" <|
            \() ->
                Expect.equal (defaultResultsign "M500x500").lane MiddleLane
        , test "lane is B lane" <|
            \() ->
                Expect.equal (defaultResultsign "B500x500").lane BLane
        , test "lane is left lane" <|
            \() ->
                Expect.equal (defaultResultsign "L500x500").lane LeftLane
        , test "lane is right lane" <|
            \() ->
                Expect.equal (defaultResultsign "R500x500").lane RightLane
        , test "lane is F lane" <|
            \() ->
                Expect.equal (getsignerrmessage "F500x500") "Could not find lane and coordinate from 'F500x500'."
        , test "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468 4 symbols" <|
            \() -> Expect.equal (List.length (defaultResultsign "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468").syms) 4
        ]


defaultResultsign : String -> EditorSign
defaultResultsign fsw =
    Result.withDefault SWEditor.EditorSign.signinit <| Debug.log "result" <| FSW.toEditorSign fsw


getsignerrmessage fsw =
    let
        result =
            FSW.toEditorSign fsw
    in
        case result of
            Ok value ->
                "Ok"

            Err msg ->
                msg
