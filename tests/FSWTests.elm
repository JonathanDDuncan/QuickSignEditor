module FSWTests exposing (..)

-- import Test exposing (..)

import Expect
import SW.FSW as FSW exposing (..)
import FSWTestHelper exposing (..)
import FSWRichTextTests exposing (..)


fswTests : Test
fswTests =
    describe "FSW Tests Suite"
        [ fswtoSignTests, fswtoRichTextTests ]


fswtoSignTests : Test
fswtoSignTests =
    describe "FSW to Sign Test Suite"
        [ test "Empty sign x 500" <|
            \() ->
                Expect.equal (defaultResultsign "M500x500").x 500
        , test "Empty sign 500x500" <|
            \() ->
                Expect.equal (getsignerrmessage "500x500") "Could not find lane from '500x500'."
        , test "Empty sign 000x500" <|
            \() ->
                Expect.equal (getsignerrmessage "000x500") "Could not find lane from '000x500'."
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
                Expect.equal (getsignerrmessage "F500x500") "Could not find lane from 'F500x500'."
        , test "world 4 symbols" <|
            \() -> Expect.equal (List.length (defaultResultsign world).syms) 4
        , test "world first symbol key S1870a" <|
            \() ->
                Expect.equal
                    (defaultResultsign world
                        |> .syms
                        |> List.head
                        |> getfield .key
                    )
                    "S1870a"
        , test "world first symbol x " <|
            \() ->
                Expect.equal
                    (defaultResultsign world
                        |> .syms
                        |> List.head
                        |> getfield (\value -> .x value |> toString)
                    )
                    "489"
        , test "world first symbol y" <|
            \() ->
                Expect.equal
                    (defaultResultsign world
                        |> .syms
                        |> List.head
                        |> getfield (\value -> .y value |> toString)
                    )
                    "515"
        , test "fsw default signinit" <|
            \() -> Expect.equal (getFsw signinit) "M500x500"
        , test "fsw default worldsign" <|
            \() -> Expect.equal (getFsw worldsign) world
        , test "roundtrip fsw with sorting sequence" <|
            \() -> Expect.equal (getFsw (defaultResultsign worldspelling)) worldspelling
        ]
