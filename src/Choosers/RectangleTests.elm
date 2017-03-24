module RectangleTests exposing (..)

import Test exposing (..)
import Expect
import SWEditor.Rectangle exposing (..)


rectangleTests : Test
rectangleTests =
    describe "Rectangle Test Suite"
        [ test "Rectangle rect1 x 0" <|
            \() ->
                Expect.equal rect1.x 0
        , test "Rectangle rect1 y 0" <|
            \() ->
                Expect.equal rect1.y 0
        , test "Rectangle rect1 width 0" <|
            \() ->
                Expect.equal rect1.width 10
        , test "Rectangle rect1 height 0" <|
            \() ->
                Expect.equal rect1.height 10
        , test "Rectangle rect2 x 0" <|
            \() ->
                Expect.equal rect2.x 0
        , test "Rectangle rect2 y 0" <|
            \() ->
                Expect.equal rect2.y 0
        , test "Rectangle rect2 width 0" <|
            \() ->
                Expect.equal rect2.width 10
        , test "Rectangle rect2 height 0" <|
            \() ->
                Expect.equal rect2.height 10
        , test "Rectangle minrectangle rect1 5 5 width" <|
            \() ->
                Expect.equal (minrectangle rect1 5 5).width 10
        , test "Rectangle minrectangle rect1 5 5 height" <|
            \() ->
                Expect.equal (minrectangle rect1 5 5).height 10
        , test "Rectangle minrectangle rect1 20 20 width" <|
            \() ->
                Expect.equal (minrectangle rect1 20 20).width 20
        , test "Rectangle minrectangle rect1 20 20 height" <|
            \() ->
                Expect.equal (minrectangle rect1 20 20).height 20
        , test "Rectangle intersect rect1 rect2" <|
            \() ->
                Expect.equal (intersect rect1 rect2) True
        , test "Rectangle intersect rect1 rect3" <|
            \() ->
                Expect.equal (intersect rect1 rect3) False
        , test "Rectangle intersect rect3 rect4" <|
            \() ->
                Expect.equal (intersect rect3 rect4) True
        ]


rect1 =
    rect 0 10 0 10


rect2 =
    rect 10 0 10 0


rect3 =
    rect 20 40 20 40


rect4 =
    rect 10 21 10 21
