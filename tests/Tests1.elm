module Tests1 exposing (..)

import Test exposing (..)
import Expect
import String
import String as String exposing (..)
import SW.SymbolConverter exposing (..)


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
              -- , test "Pua char1" <|
              --     \() ->
              --         Expect.equal (puaChar1 1038475 ++ puaChar1 1038353 ++ puaChar1 1038383) "\xFD88B\xFD811\xFD82F"
              -- , test "Pua char1" <|
              --     \() ->
              --         Expect.equal (puaChar1 1038353) "\xFD811"
            , test "Pua lefthalf" <|
                \() ->
                    Expect.equal (lefthalf 1038353) 56246
            , test "Pua righthalf" <|
                \() ->
                    Expect.equal (righthalf 1038353) 56337
              -- , test "Pua char" <|
              --     \() ->
              --         Expect.equal (puaChar) "\xFD90E"
              -- , test "Pua char1" <|
              --     \() ->
              --         Expect.equal (puaChar1 1038606) "\xFD90E"
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
