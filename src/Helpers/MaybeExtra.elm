module Helpers.MaybeExtra exposing (..)


isNothing : Maybe a -> Bool
isNothing val =
    case val of
        Just v ->
            False

        Nothing ->
            True
