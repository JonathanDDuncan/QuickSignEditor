module FSWParserTests exposing (..)

import Test exposing (..)
import Expect
import SW.FSWParser as FSWParser exposing (..)


-- import SW.Types exposing (..)
-- import FSWTestHelper exposing (..)
-- import FSWRichTextTests exposing (..)

import Parser exposing (..)


fswParserTests : Test
fswParserTests =
    describe "FSWParser Tests Suite"
        [ test "Parse coordinate"
            (\() ->
                Expect.equal
                    (case (run coordinate "6X2") of
                        Ok value ->
                            value

                        Err _ ->
                            { x = 0, y = 0 }
                    )
                    { x = 6, y = 2 }
            )
        ]
