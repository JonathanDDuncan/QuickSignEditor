module FSWRichTextTests exposing (..)

import Test exposing (..)
import Expect
import SW.Types exposing (..)
import FSWTestHelper exposing (..)


fswtoRichTextTests : Test
fswtoRichTextTests =
    describe "FSW to RichText Test Suite"
        [ fswRichTextFullSignColorsTests
        , fswRichTextSpecificSymbolColorsTests
        , fswRichTextSymbolZoomLevelTests
        ]


fswRichTextFullSignColorsTests : Test
fswRichTextFullSignColorsTests =
    describe "FSW to RichText Full Sign Colors Test Suite"
        [ test "Signsymbols normally black first is now blue" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldbluegreen).syms
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nbcolor
                    )
                    "blue"
        , test "Signsymbols normally white first is now green" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldbluegreen).syms
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nwcolor
                    )
                    "green"
        , test "Signsymbols normally black is now blue" <|
            \() ->
                Expect.equal (allsymbolscolor .nbcolor "blue" (defaultResultsign worldbluegreen)) True
        , test "Signsymbols normally white is now green" <|
            \() ->
                Expect.equal (allsymbolscolor .nwcolor "green" (defaultResultsign worldbluegreen)) True
        , test "Signsymbols normally black first is now #f44242" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldrgbredmagenta).syms
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nbcolor
                    )
                    "#f44242"
        , test "Signsymbols normally white first is now #f441ee" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldrgbredmagenta).syms
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nwcolor
                    )
                    "#f441ee"
        , test "Signsymbols normally black is now #f44242" <|
            \() ->
                Expect.equal (allsymbolscolor .nbcolor "#f44242" (defaultResultsign worldrgbredmagenta)) True
        , test "Signsymbols normally white is now #f441ee" <|
            \() ->
                Expect.equal (allsymbolscolor .nwcolor "#f441ee" (defaultResultsign worldrgbredmagenta)) True
        ]


fswRichTextSpecificSymbolColorsTests : Test
fswRichTextSpecificSymbolColorsTests =
    describe "FSW to RichText Specific Symbol Colors Test Suite"
        [ test "third symbol normally black is blue" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldthirdbluegreen).syms
                        |> List.drop 2
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nbcolor
                    )
                    "blue"
        , test "besides third symbol normally black is black" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldthirdbluegreen).syms
                        |> (\mylist ->
                                List.append (List.take 2 mylist) (List.drop 3 mylist)
                           )
                        |> List.all (\symbol -> symbol.nbcolor == "black")
                    )
                    True
        , test
            "third symbol normally white is green"
          <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldthirdbluegreen).syms
                        |> List.drop 2
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nwcolor
                    )
                    "green"
        , test "besides third symbol normally white is white" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldthirdbluegreen).syms
                        |> (\mylist ->
                                List.append (List.take 2 mylist) (List.drop 3 mylist)
                           )
                        |> List.all (\symbol -> symbol.nwcolor == "white")
                    )
                    True
        , test "second symbol normally black is #f44242" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldsecondrgbredmagenta).syms
                        |> List.drop 1
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nbcolor
                    )
                    "#f44242"
        , test "besides second symbol normally black is black" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldsecondrgbredmagenta).syms
                        |> (\mylist ->
                                List.append (List.take 1 mylist) (List.drop 4 mylist)
                           )
                        |> List.all (\symbol -> symbol.nbcolor == "black")
                    )
                    True
        , test
            "second symbol normally white is #f441ee"
          <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldsecondrgbredmagenta).syms
                        |> List.drop 1
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .nwcolor
                    )
                    "#f441ee"
        , test "besides second symbol normally white is white" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldsecondrgbredmagenta).syms
                        |> (\mylist ->
                                List.append (List.take 1 mylist) (List.drop 4 mylist)
                           )
                        |> List.all (\symbol -> symbol.nwcolor == "white")
                    )
                    True
        ]


fswRichTextSymbolZoomLevelTests : Test
fswRichTextSymbolZoomLevelTests =
    describe "FSW to RichText Symbol Zoom Level Test Suite"
        [ test "second symbol zoom level 2.3" <|
            \() ->
                Expect.equal
                    ((defaultResultsign worldsecondzoomdoubleadjusted).syms
                        |> List.drop 2
                        |> List.head
                        |> Maybe.withDefault symbolinit
                        |> .size
                    )
                    2.3
        ]
