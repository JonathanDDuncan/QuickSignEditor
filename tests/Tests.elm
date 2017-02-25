module Tests exposing (..)

import Test exposing (..)
import SymbolConverterTests exposing (..)
import FSWTests exposing (..)


all : Test
all =
    describe "All Test Suite"
        [ symbolConverterTests
        , fswTests
        ]
