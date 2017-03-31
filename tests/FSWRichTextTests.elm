module FSWRichTextTests exposing (..)

import Test exposing (..)
import Expect
import SW.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
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
                Expect.equal (allsymbolscolor "blue" (defaultResultsign worldblue)) True
        ]


worldblue =
    world ++ "-D_blue_"


allsymbolscolor : String -> Sign -> Bool
allsymbolscolor color sign =
    List.all (\symbol -> (Debug.log "nbcolor" <| symbol.nbcolor) == color) sign.syms
