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
                Expect.equal (extractvalue (run coordinate "62X 2")    { x = 0  }
                -- { x = 0, y = 0 }
                 )

                    { x = 62 }
                    -- { x = 6, y = 2 }
            )
        ]

extractvalue result default  =
   case result of
      Ok value ->
          value

      Err err ->
          let
              a =  Debug.log "" err
          in
          default
