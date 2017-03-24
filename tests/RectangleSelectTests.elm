module RectangleSelectTests exposing (..)

import Test exposing (..)
import Expect
import SWEditor.Rectangle exposing (..)
import SWEditor.RectangleSelect exposing (..)
import SW.Types exposing (Symbol, Sign)
import SWEditor.Types exposing (..)
import SWEditor.State exposing (..)


rectangleselectTests : Test
rectangleselectTests =
    describe "RectangleSelect Test Suite"
        [ test "RectangleSelect rectangleStartCurrent model1 " <|
            \() ->
                Expect.equal currentRectangle { x = 30, y = 30, width = 45, height = 45 }
        , test "RectangleSelect (selectIntersected currentRectangle symbol1).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol1).selected True
        , test "RectangleSelect (selectIntersected currentRectangle symbol2).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol2).selected False
        , test "RectangleSelect (selectIntersected currentRectangle symbol3).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol3).selected True
        , test "RectangleSelect (selectIntersected currentRectangle symbol4).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol4).selected False
        , test "RectangleSelect (selectIntersected currentRectangle symbol5).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol5).selected True
        , test "RectangleSelect (selectIntersected currentRectangle symbol6).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol6).selected False
        , test "RectangleSelect (selectIntersected currentRectangle symbol7).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol7).selected True
        , test "RectangleSelect (selectIntersected currentRectangle symbol8).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol8).selected False
        , test "RectangleSelect (selectIntersected currentRectangle symbol9).selected" <|
            \() ->
                Expect.equal (selectIntersected currentRectangle symbol9).selected True
        ]


symbol1 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 40, y = 40, width = 30, height = 30 }


symbol2 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 10, y = 30, width = 20, height = 20 }


symbol3 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 11, y = 30, width = 20, height = 20 }


symbol4 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 30, y = 10, width = 20, height = 20 }


symbol5 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 30, y = 11, width = 20, height = 20 }


symbol6 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 75, y = 30, width = 20, height = 20 }


symbol7 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 74, y = 30, width = 20, height = 20 }


symbol8 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 30, y = 75, width = 20, height = 20 }


symbol9 =
    let
        initialsymbol =
            SW.Types.symbolinit
    in
        { initialsymbol | x = 30, y = 74, width = 20, height = 20 }


currentRectangle =
    (rectangleStartCurrent model1)


model1 =
    let
        rectanglestart =
            { x = 30, y = 30 }

        currentxy =
            { x = 75, y = 75 }

        initial =
            Tuple.first init
    in
        { initial | rectanglestart = rectanglestart, xy = currentxy }
