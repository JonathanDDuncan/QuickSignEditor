module Convert.ConvertFsw exposing (..)

import ParseInt as ParseInt exposing (..)
import String as String exposing (..)


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
    "S" ++ (ParseInt.toRadix' 16 base) ++ (ParseInt.toRadix' 16 (fill - 1)) ++ (ParseInt.toRadix' 16 (rotation - 1))


keyfromcode : Int -> String
keyfromcode code =
    key (base code) (fill code) (rotation code)


hexbaseFromKey : String -> Int
hexbaseFromKey key =
    (ParseInt.parseIntHex (slice 1 4 key)) |> Result.toMaybe |> Maybe.withDefault 0


hexfillFromKey : String -> Int
hexfillFromKey key =
    ((ParseInt.parseIntHex (slice 4 5 key)) |> Result.toMaybe |> Maybe.withDefault 0) + 1


hexrotationFromKey : String -> Int
hexrotationFromKey key =
    ((ParseInt.parseIntHex (slice 5 6 key)) |> Result.toMaybe |> Maybe.withDefault 0) + 1


codefromkey : String -> Int
codefromkey key =
    code (hexbaseFromKey key) (hexfillFromKey key) (hexrotationFromKey key)
