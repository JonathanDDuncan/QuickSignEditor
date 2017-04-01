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
        , test "third symbol blue" <|
            \() ->
                Expect.equal
                    ((Debug.log "sign " <| defaultResultsign worldthirdblue).syms
                        |> List.drop 2
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nbcolor
                    )
                    "blue"
        , test "third symbol blue others black" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldthirdblue).syms
                        |> (\mylist ->
                                List.append (List.take 2 mylist) (List.drop 3 mylist)
                           )
                        |> List.all (\symbol -> symbol.nbcolor == "black")
                    )
                    True
        ]
