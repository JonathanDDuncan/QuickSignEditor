module MainChooser.Types exposing (..)

-- import SubMainChoosers.Types

import Choosing.Types as Choosing
import SWEditor.Types exposing (..)
import SW.Types exposing (..)


type alias Model =
    { choosings :
        List Choosing.Model
    , handgroupchoosings : HandGroupModel
    , allgroupchoosings : AllGroupChoosings
    , clicked : String
    , selectedcolumn : Int
    , groupselected : ChooserItem
    , handgroupfilter : Int
    }


type alias HandGroupImportModel =
    { chooseritemvalues : List ChooserItemValue
    , basechooseritems : List BaseChooserItem
    }


type alias ChooserItemValue =
    { choosertype : String
    , name : String
    , value : Int
    , symbolgroup : String
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
    , subgroup1 : String
    , subgroup2 : String
    , plane : String
    , rank : Int
    }


type alias ChooserItem =
    { base : Int
    , name : String
    , symbolid : String
    , symbolkey : String
    , unicodepua : String
    , validfills : String
    , validrotations : String
    , groupchooser : Int
    , common : Bool
    , subgroup1 : Int
    , subgroup2 : Int
    , plane : Int
    , rank : Int
    }


chooseriteminit =
    { base = 256
    , name = "Index"
    , symbolid = "01-01-001-01"
    , symbolkey = "S100"
    , unicodepua = "U+FD830"
    , validfills = "1 - 6"
    , validrotations = "1 - 16"
    , groupchooser = 1
    , common = True
    , subgroup1 = 1
    , subgroup2 = 2
    , plane = 1
    , rank = 1
    }
type alias HandGroupModel =
    List ChooserItem


type alias AllGroupChoosings =
    List GroupChoosing


type alias GroupChoosing =
    { basesymbol : String
    , choosings : List ChooserItem
    }


type Msg
    = MainChooserMessage
    | RequestInitialChoosings
    | ReceiveInitialChoosings (List Choosing.ImportModel)
    | ReceiveInitialGroupHandChoosings HandGroupImportModel
    | Choosing Choosing.Msg
    | Clicked String
    | SymbolView SWEditor.Types.Msg
    | SignView SWEditor.Types.Msg
    | SelectedColumn Int
    | GroupSelected ChooserItem
    | DragSymbol Code
    | FilterHandGroup Int



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
