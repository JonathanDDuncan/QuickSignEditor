module Tests exposing (..)

import Test exposing (..)
import SymbolConverterTests exposing (..)


all : Test
all =
    describe "All Test Suite"
        [ symbolConverterTests
        ]
