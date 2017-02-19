module MaybeHelper.MaybeExtra exposing (..)


removemaybe : a -> List (Maybe a) -> List a
removemaybe default list =
    List.map
        (\d ->
            Maybe.withDefault default d
        )
    <|
        List.filter
            (\d ->
                case d of
                    Just symb ->
                        True

                    Nothing ->
                        False
            )
            list