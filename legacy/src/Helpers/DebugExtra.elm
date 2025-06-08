module Helpers.DebugExtra exposing (debugkey)


debugkey : comparable -> String -> a -> a
debugkey n message value =
    if n == 2 then
        Debug.log message value
    else
        value
