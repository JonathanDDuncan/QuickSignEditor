module SW.FSWParser exposing (..)

import Regex exposing (..)
import SWEditor.EditorSymbol exposing (getSymbolbyKey)
import SWEditor.EditorSign exposing (centerSign, colorallsymbols, colorsymbols, sizesymbols, adjustpositionsymbols)
import Dict
import SW.Types exposing (..)
import Helpers.ResultExtra exposing (..)
import  Parser exposing (..)

coordinate  =
   succeed Coordinate
    |=    token ExpectingSymbol  int

    -- |. symbol "X"
    --
    -- |=    int



type alias Coordinate = {x :Int}
  -- {x :Int, y:Int}
