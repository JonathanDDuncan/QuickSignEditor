module Tests1 exposing (..)

import Test exposing (..)
import Expect
import String
import ParseInt as ParseInt exposing (..)
import String as String exposing (..)
import Char as Char exposing (..)
import Bitwise as Bitwise exposing (..)


dectoHex : Int -> String
dectoHex value =
    let
        val =
            ParseInt.toRadix 16 value
    in
        case val of
            Err msg ->
                ""

            Ok str ->
                str


hextoDec : String -> Int
hextoDec value =
    ParseInt.parseIntHex value |> Result.toMaybe |> Maybe.withDefault 0


lefthalf value =
    hextoDec "D800" + (Bitwise.shiftRightBy 10 (value - (hextoDec "10000")))


righthalf value =
    hextoDec "DC00" + (Bitwise.and (value - (hextoDec "10000")) (hextoDec "03FF"))


puaChar1 value =
    String.fromChar (Char.fromCode <| lefthalf value) ++ String.fromChar (Char.fromCode <| righthalf value)


puaChar =
    String.fromChar (Char.fromCode 56246) ++ String.fromChar (Char.fromCode 56590)


hexbaseFromKey : String -> Int
hexbaseFromKey key =
    hextoDec (slice 1 4 key)


hexfillFromKey : String -> Int
hexfillFromKey key =
    hextoDec (slice 4 5 key)


hexrotationFromKey : String -> Int
hexrotationFromKey key =
    hextoDec (slice 5 6 key)


puabasestart : Int
puabasestart =
    hextoDec "FD730"


puafillstart : Int
puafillstart =
    hextoDec "FD810"


puarotationstart : Int
puarotationstart =
    hextoDec "FD820"


puaCharCode : Int -> String
puaCharCode value =
    puaChar1 value


code : Int -> Int -> Int -> Int
code base fill rotation =
    0x00100000 + 96 * (base - 256) + 16 * (fill) + rotation + 1


pua : String -> String
pua key =
    let
        base =
            hexbaseFromKey key

        puabase =
            base + puabasestart

        fill =
            hexfillFromKey key

        puafill =
            fill + puafillstart

        rotation =
            hexrotationFromKey key

        puarotation =
            rotation + puarotationstart
    in
        puaCharCode (puabase) ++ puaCharCode (puafill) ++ puaCharCode (puarotation)


rotation : Int -> Int
rotation code =
    (code - 0x00100000) % 16


fill : Int -> Int
fill code =
    ((code - 0x00100000) // 16) % 6 + 1


base : Int -> Int
base code =
    ((code - 0x00100000) // 96) + 256


key : Int -> Int -> Int -> String
key base fill rotation =
    "S" ++ dectoHex base ++ dectoHex (fill - 1) ++ dectoHex (rotation - 1)


keyfromcode : Int -> String
keyfromcode code =
    key (base code) (fill code) (rotation code)


all1 : Test
all1 =
    let
        value =
            20
    in
        describe "A Test Suite"
            [ test "Code 1061538 " <|
                \() ->
                    Expect.equal (keyfromcode 1061538) "S18701"
            , test "Code S31430 " <|
                \() ->
                    Expect.equal (code (hexbaseFromKey "S31430") (hexfillFromKey "S31430") (hexrotationFromKey "S31430")) 1099697
            , test "Pua S10000 base" <|
                \() ->
                    Expect.equal (hexbaseFromKey "S10000") 256
            , test "Pua S10000 fill" <|
                \() ->
                    Expect.equal (hexfillFromKey "S10000") 0
            , test "Pua S10000 rotation" <|
                \() ->
                    Expect.equal (hexrotationFromKey "S10000") 0
            , test "Pua S10000 puabase" <|
                \() ->
                    Expect.equal (hexbaseFromKey "S10000" + puabasestart) 1038384
            , test "Pua S10000 puafill" <|
                \() ->
                    Expect.equal (hexfillFromKey "S10000" + puafillstart) 1038352
            , test "Pua S10000 puarotation" <|
                \() ->
                    Expect.equal (hexrotationFromKey "S10000" + puarotationstart) 1038368
            , test "Pua S10000" <|
                \() ->
                    Expect.equal (pua "S10000") "\xFD830\xFD810\xFD820"
            , test "Pua S10000" <|
                \() ->
                    Expect.equal (pua "S10000") "\xFD830\xFD810\xFD820"
            , test "Pua char1" <|
                \() ->
                    Expect.equal (puaChar1 1038475 ++ puaChar1 1038353 ++ puaChar1 1038383) "\xFD88B\xFD811\xFD82F"
            , test "Pua char1" <|
                \() ->
                    Expect.equal (puaChar1 1038353) "\xFD811"
            , test "Pua lefthalf" <|
                \() ->
                    Expect.equal (lefthalf 1038353) 56246
            , test "Pua righthalf" <|
                \() ->
                    Expect.equal (righthalf 1038353) 56337
            , test "Pua char" <|
                \() ->
                    Expect.equal (puaChar) "\xFD90E"
            , test "Pua char1" <|
                \() ->
                    Expect.equal (puaChar1 1038606) "\xFD90E"
            , test "Pua lefthalf" <|
                \() ->
                    Expect.equal (lefthalf 1038606) 56246
            , test "Pua righthalf" <|
                \() ->
                    Expect.equal (righthalf 1038606) 56590
            , test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
              -- , test "This test should fail" <|
              --     \() ->
              --         Expect.fail "failed as expected!"
            ]
