module Choosers.ChooserItemType exposing (ChooserItem, chooseriteminit)

import SW.Pua exposing (Fill, Base, Key)


type alias ChooserItem =
    { base : Base
    , name : String
    , symbolid : String
    , symbolkey : String
    , unicodepua : String
    , validfills : String
    , validrotations : String
    , groupchooser : Int
    , common : Bool
    , feature : Int
    , row : Int
    , col : Int
    , rank : Int
    }


chooseriteminit : ChooserItem
chooseriteminit =
    { base = 256
    , name = "Index"
    , symbolid = "01-01-001-01"
    , symbolkey = ""
    , unicodepua = "U+FD830"
    , validfills = "1 - 6"
    , validrotations = "1 - 16"
    , groupchooser = 1
    , common = True
    , feature = 0
    , row = 1
    , col = 1
    , rank = 1
    }
