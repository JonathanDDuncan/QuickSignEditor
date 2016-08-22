module DisplaySW.Rest exposing (..)

-- import Http exposing (Error)
-- import Json.Decode exposing (..)
-- import Task
-- import Types exposing (..)
-- import DisplaySW.Types exposing (..)

import Json.Encode
import Json.Decode exposing ((:=))
import DisplaySW.Types exposing (..)
import Json.Decode.Extra exposing ((|:))
import SW.Types exposing (..)


decodeSign : Json.Decode.Decoder Sign
decodeSign =
    Json.Decode.succeed Sign
        |: ("width" := Json.Decode.int)
        |: ("height" := Json.Decode.int)
        |: ("text" := Json.Decode.string)
        |: ("x" := Json.Decode.int)
        |: ("y" := Json.Decode.int)
        |: ("backfill" := Json.Decode.string)
        |: ("syms" := Json.Decode.list decodeSymbol)
        |: ("laned" := Json.Decode.bool)
        |: ("left" := Json.Decode.int)


encodeSign : Sign -> Json.Encode.Value
encodeSign record =
    Json.Encode.object
        [ ( "width", Json.Encode.int <| record.width )
        , ( "height", Json.Encode.int <| record.height )
        , ( "text", Json.Encode.string <| record.text )
        , ( "x", Json.Encode.int <| record.x )
        , ( "y", Json.Encode.int <| record.y )
        , ( "backfill", Json.Encode.string <| record.backfill )
        , ( "syms", Json.Encode.list <| List.map encodeSymbol <| record.syms )
        , ( "laned", Json.Encode.bool <| record.laned )
        , ( "left", Json.Encode.int <| record.left )
        ]


decodeSymbol : Json.Decode.Decoder Symbol
decodeSymbol =
    Json.Decode.succeed Symbol
        |: ("x" := Json.Decode.int)
        |: ("y" := Json.Decode.int)
        |: ("fontsize" := Json.Decode.int)
        |: ("nwcolor" := Json.Decode.string)
        |: ("pua" := Json.Decode.string)
        |: ("code" := Json.Decode.int)
        |: ("key" := Json.Decode.string)
        |: ("nbcolor" := Json.Decode.string)


encodeSymbol : Symbol -> Json.Encode.Value
encodeSymbol record =
    Json.Encode.object
        [ ( "x", Json.Encode.int <| record.x )
        , ( "y", Json.Encode.int <| record.y )
        , ( "fontsize", Json.Encode.int <| record.fontsize )
        , ( "nwcolor", Json.Encode.string <| record.nwcolor )
        , ( "pua", Json.Encode.string <| record.pua )
        , ( "code", Json.Encode.int <| record.code )
        , ( "key", Json.Encode.string <| record.key )
        , ( "nbcolor", Json.Encode.string <| record.nbcolor )
        ]
