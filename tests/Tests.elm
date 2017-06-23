module Tests exposing (..)

import Test exposing (..)
import PuaTests exposing (..)
import RectangleTests exposing (..)
import RectangleSelectTests exposing (..)


all : Test
all =
    describe "All Test Suite"
        [ rectangleselectTests
        , rectangleTests
        , puaTests
        ]
