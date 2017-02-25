module FSWTests exposing (..)

import Test exposing (..)
import Expect
import SW.FSW as FSW exposing (..)


fswTests : Test
fswTests =
    describe "FSW to EditorSign Test Suite"
        [ test "Empty sign x 500" <|
            \() ->
                Expect.equal (FSW.toEditorSign "M500x500").x 500
        , test "Empty sign y 500" <|
            \() ->
                Expect.equal (FSW.toEditorSign "M500x500").y 500
        , test "Empty sign x 498" <|
            \() ->
                Expect.equal (FSW.toEditorSign "M498x497").x 498
        , test "Empty sign y 497" <|
            \() ->
                Expect.equal (FSW.toEditorSign "M498x497").y 497
        , test "Empty sign no symbols" <|
            \() ->
                Expect.equal (FSW.toEditorSign "M500x500").syms []
          --  create lane for EditorSign for and Sign for full FSW both way converssion
          -- , test "lane is middle" <|
          --     \() ->
          --         Expect.equal (FSW.toEditorSign "M500x500").lane "middle"
        ]
