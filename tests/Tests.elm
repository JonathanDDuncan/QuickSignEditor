module Tests exposing (..)

import Test exposing (..)
import PuaTests exposing (..)
import FSWTests exposing (..)


all : Test
all =
    describe "All Test Suite"
        [ puaTests
        , fswTests
        ]
