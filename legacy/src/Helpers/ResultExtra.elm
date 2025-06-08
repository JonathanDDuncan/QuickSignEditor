module Helpers.ResultExtra exposing (andThentoResult, toResult)


andThentoResult : (a -> value) -> Result error a -> Result error value
andThentoResult action value =
    Result.andThen (toResult action) value


toResult : (a -> value) -> a -> Result error value
toResult action value =
    action value
        |> Ok
