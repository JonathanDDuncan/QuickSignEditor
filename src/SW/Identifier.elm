module SW.Identifier exposing (updateId, updateIds, lastid)

-- updateIds : Int -> List Symbol -> List Symbol


updateIds : Int -> List { b | id : a } -> List { b | id : Int }
updateIds startid items =
    List.indexedMap (updateId startid) items


updateId : Int -> Int -> { b | id : a } -> { b | id : Int }
updateId startid index item =
    { item
        | id = startid + index + 1
    }


lastid : List { a | id : Int } -> Int
lastid symbols =
    case maximumBy .id symbols of
        Nothing ->
            0

        Just sign ->
            sign.id


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f ls =
    let
        maxBy f1 x y =
            if f1 x > f1 y then
                x
            else
                y
    in
        case ls of
            l_ :: ls_ ->
                Just <| List.foldl (maxBy f) l_ ls_

            _ ->
                Nothing
