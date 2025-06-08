module PuaTests exposing (..)

import Test exposing (..)
import Expect
import String as String exposing (..)
import SW.Pua exposing (..)


puaTests : Test
puaTests =
    describe "A Test Suite"
        [ test "Pua S10000 base" <|
            \() ->
                Expect.equal (hexbaseFromKey "S10000") 256
        , test "Pua S10000 fill" <|
            \() ->
                Expect.equal (hexfillFromKey "S10000") 0
        , test "Pua S10000 rotation" <|
            \() ->
                Expect.equal (hexrotationFromKey "S10000") 0
        , test "Pua lefthalf" <|
            \() ->
                Expect.equal (lefthalf 1038353) 56246
        , test "Pua righthalf" <|
            \() ->
                Expect.equal (righthalf 1038353) 56337
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
        , test "fillcodefromkey S1870a" <|
            \() ->
                Expect.equal (puaCharCode <| fillcodefromkey "S1870a")  "\x1032AB"
        , test "linecodefromkey S1870a" <|
            \() ->
                Expect.equal (puaCharCode <| linecodefromkey "S1870a")  "\x432AB"
        ]
