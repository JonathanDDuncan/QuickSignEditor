module SW.PortableSign exposing (PortableSign, portableSigntoSign, PortableSymbol)

import SW.Symbol exposing (Symbol)
import SW.Sign exposing (Sign, Lane(MiddleLane), getlane)


type alias PortableSign =
    { lane : String
    , width : Int
    , height : Int
    , x : Int
    , y : Int
    , backfill : String
    , syms : List PortableSymbol
    }


portableSigntoSign : PortableSign -> Sign
portableSigntoSign portableSign =
    { lane = getlane portableSign.lane |> Maybe.withDefault MiddleLane
    , width = portableSign.width
    , height = portableSign.height
    , x = portableSign.x
    , y = portableSign.y
    , backfill = portableSign.backfill
    , syms = List.map portableSymboltoSymbol portableSign.syms
    , spelling = ""
    }


type alias PortableSymbol =
    { key : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , size : Float
    , nwcolor : Maybe String
    , nbcolor : Maybe String
    }


portableSymboltoSymbol : PortableSymbol -> Symbol
portableSymboltoSymbol portablesymbol =
    { key = portablesymbol.key
    , id = 0
    , selected = False
    , x = portablesymbol.x
    , y = portablesymbol.y
    , width = portablesymbol.width
    , height = portablesymbol.height
    , size = portablesymbol.size
    , nwcolor =
        case portablesymbol.nwcolor of
            Just color ->
                color

            Nothing ->
                "white"
    , nbcolor =
        case portablesymbol.nbcolor of
            Just color ->
                color

            Nothing ->
                "black"
    }
