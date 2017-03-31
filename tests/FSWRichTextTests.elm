module FSWRichTextTests exposing (..)

import Test exposing (..)
import Expect
import SW.Types exposing (..)
import FSWTestHelper exposing (..)


fswtoRichTextTests : Test
fswtoRichTextTests =
    describe "FSW to RichText Test Suite"
        [ test "Signsymbols first blue" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldblue).syms
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nbcolor
                    )
                    "blue"
        , test "Signsymbols blue" <|
            \() ->
                Expect.equal (allsymbolsnbcolor "blue" (defaultResultsign worldblue)) True
        ]
