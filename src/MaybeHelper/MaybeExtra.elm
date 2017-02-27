module MaybeHelper.MaybeExtra exposing (..)


removeNothings : List (Maybe a) -> List a
removeNothings list =
    List.filterMap (\value -> value) list
