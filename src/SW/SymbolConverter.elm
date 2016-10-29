module SW.SymbolConverter exposing (..)

import ParseInt as ParseInt exposing (..)
import String as String exposing (..)
import Char as Char exposing (..)
import Bitwise as Bitwise exposing (..)


rotation : Int -> Int
rotation code =
    code % 16


fill : Int -> Int
fill code =
    (code // 16) % 6 + 1


base : Int -> Int
base code =
    (code // 96) + 256


code : Int -> Int -> Int -> Int
code base fill rotation =
    96 * (base - 256) + 16 * (fill - 1) + rotation


key : Int -> Int -> Int -> String
key base fill rotation =
    "S" ++ dectoHex base ++ dectoHex (fill - 1) ++ dectoHex (rotation - 1)
--  "S" ++ (ParseInt.toRadix' 16 base) ++ (ParseInt.toRadix' 16 (fill - 1)) ++ (ParseInt.toRadix' 16 (rotation - 1))

dectoHex : Int -> String
dectoHex value =
    ParseInt.toRadix' 16 value


hextoDec : String -> Int
hextoDec value =
    ParseInt.parseIntHex value |> Result.toMaybe |> Maybe.withDefault 0


keyfromcode : Int -> String
keyfromcode code =
    key (base code) (fill code) (rotation code)


hexbaseFromKey : String -> Int
hexbaseFromKey key =
    hextoDec (slice 1 4 key)


hexfillFromKey : String -> Int
hexfillFromKey key =
    hextoDec (slice 4 5 key)


hexrotationFromKey : String -> Int
hexrotationFromKey key =
    hextoDec (slice 5 6 key)


codefromkey : String -> Int
codefromkey key =
    code (hexbaseFromKey key) (hexfillFromKey key) (hexrotationFromKey key)


puabasestart : Int
puabasestart =
    hextoDec "FD730"


puafillstart : Int
puafillstart =
    hextoDec "FD810"


puarotationstart : Int
puarotationstart =
    hextoDec "FD820"


pua : String -> String
pua key =
    let
        base =
            hexbaseFromKey key

        puabase =
            base + puabasestart

        fill =
            hexfillFromKey key

        puafill =
            fill + puafillstart

        rotation =
            hexrotationFromKey key

        puarotation =
            rotation + puarotationstart
    in
        puaCharCode (puabase) ++ puaCharCode (puafill) ++ puaCharCode (puarotation)



puahextext : String -> String
puahextext key =
    let
        base =
            Debug.log "hexbaseFromKey key" (hexbaseFromKey key)

        puabase =
            base + puabasestart

        fill =
            hexfillFromKey key

        puafill =
            fill + puafillstart

        rotation =  
            hexrotationFromKey key

        puarotation =
            rotation + puarotationstart
    in
        dectoHex puabase ++ dectoHex puafill ++ dectoHex puarotation


lefthalf value =
    hextoDec "D800" + (Bitwise.shiftRight (value - (hextoDec "10000")) 10)


righthalf value =
    hextoDec "DC00" + (Bitwise.and (value - (hextoDec "10000")) (hextoDec "03FF"))


puaChar1 value =
    String.fromChar (Char.fromCode <| lefthalf value) ++ String.fromChar (Char.fromCode <| righthalf value)


puaCharCode : Int -> String
puaCharCode value =
    puaChar1 value
