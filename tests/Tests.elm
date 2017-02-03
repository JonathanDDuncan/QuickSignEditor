module Tests exposing (..)

import Test exposing (..)
import Tests1 exposing (..)


all : Test
all =
    describe "All Test Suite"
        [ all1
        ]
