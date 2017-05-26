module SW.FSWParser exposing (..)

import Regex exposing (..)
import SWEditor.EditorSymbol exposing (getSymbolbyKey)
import SWEditor.EditorSign exposing (centerSign, colorallsymbols, colorsymbols, sizesymbols, adjustpositionsymbols)
import Dict
import SW.Types exposing (..)
import Helpers.ResultExtra exposing (..)
import  Parser exposing (..)

a =
   5

coordinate  =
   succeed Coordinate

    |= int

    |. symbol "X"

    |= int



type alias Coordinate =  {x :Int, y:Int}
