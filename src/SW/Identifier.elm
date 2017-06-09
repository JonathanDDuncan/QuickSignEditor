module SW.Identifier exposing (..)

-- updateIds : Int -> List Symbol -> List Symbol


updateIds : Int -> List { b | id : a } -> List { b | id : Int }
updateIds startid items =
    List.indexedMap (updateId startid) items


updateId : Int -> Int -> { b | id : a } -> { b | id : Int }
updateId startid index item =
    { item
        | id = startid + index + 1
    }
