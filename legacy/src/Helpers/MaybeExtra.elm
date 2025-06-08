module Helpers.MaybeExtra exposing (isNothing, removeNothings)


isNothing : Maybe a -> Bool
isNothing val =
    case val of
        Just _ ->
            False

        Nothing ->
            True


removeNothings : List (Maybe a) -> List a
removeNothings list =
    List.filterMap (\value -> value) list
