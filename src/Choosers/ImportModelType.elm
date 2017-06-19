module Choosers.ImportModelType exposing (HandGroupImportModel, ChoosingImportModel, BaseChooserItem, ChooserItemValue)

import SW.PortableSign exposing (PortableSign)
import SW.Symbol exposing (Symbol, HandFills(..), Hands(..), Planes(..))
import SWEditor.Types exposing (Model, Msg, Offset)


type alias HandGroupImportModel =
    { chooseritemvalues : List ChooserItemValue
    , basechooseritems : List BaseChooserItem
    , symbolsizes : List SymbolSize
    }


type alias ChoosingImportModel =
    { displaySign : PortableSign
    , valuestoAdd : List Symbol
    , value : String
    , offset : Offset
    }


type alias ChooserItemValue =
    { choosertype : String
    , name : String
    , value : Int
    , symbolgroup : String
    }


type alias SymbolSize =
    { k : String
    , h : Int
    , w : Int
    }


type alias BaseChooserItem =
    { base : Int
    , name : String
    , symbolid : String
    , symbolkey : String
    , unicodepua : String
    , validfills : String
    , validrotations : String
    , groupchooser : String
    , common : Bool
    , feature : String
    , rowname : String
    , colname : String
    , rank : Int
    }
