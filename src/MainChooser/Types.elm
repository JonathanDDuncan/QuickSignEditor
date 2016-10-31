module MainChooser.Types exposing (..)

-- import SubMainChoosers.Types

import Choosing.Types as Choosing
import SWEditor.Types exposing (..)


type alias Model =
    { choosings :
        List Choosing.Model
        -- , handgroupchoosings : HandGroupModel
    , clicked : String
    , selectedcolumn : Int
    }



-- type alias HandGroupImportModel =
--     { fistbabycommon : List Choosing.ImportModel
--     , fistringcommon : List Choosing.ImportModel
--     , fistmiddlecommon : List Choosing.ImportModel
--     , fistindexcommon : List Choosing.ImportModel
--     , fistthumbcommon : List Choosing.ImportModel
--     , circlethumbcommon : List Choosing.ImportModel
--     , circleindexcommon : List Choosing.ImportModel
--     , circleringcommon : List Choosing.ImportModel
--     , circlebabycommon : List Choosing.ImportModel
--     , cupbabycommon : List Choosing.ImportModel
--     , cupthumbcommon : List Choosing.ImportModel
--     , cupindexcommon : List Choosing.ImportModel
--     , anglethumbcommon : List Choosing.ImportModel
--     , anglebabycommon : List Choosing.ImportModel
--     , flatthumbcommon : List Choosing.ImportModel
--     , flatbabycommon : List Choosing.ImportModel
--     }


type alias HandGroupImportModel =
    { chooseritemvalues : List ChooserItemValue
    , basechooseritems : List BaseChooserItem
    }


type alias ChooserItemValue =
    { choosertype : String
    , name : String
    , value : Int
    }
 
     
type alias BaseChooserItem =
    { base : Int
    , name : String
    , symbolid : String
    , symbolkey : String
    , unicodepua : String
    , validfills : String
    , validrotations : String
    , handpng : String
    , symbolpng : String
    , groupchooser : String
    , common : Bool
    , subgroup1 : String
    , subgroup2 : String
    , rank : Int
    } 

    

-- type alias HandGroupModel =
--     { fistbabycommon : List Choosing.Model
--     , fistringcommon : List Choosing.Model
--     , fistmiddlecommon : List Choosing.Model
--     , fistindexcommon : List Choosing.Model
--     , fistthumbcommon : List Choosing.Model
--     , circlethumbcommon : List Choosing.Model
--     , circleindexcommon : List Choosing.Model
--     , circleringcommon : List Choosing.Model
--     , circlebabycommon : List Choosing.Model
--     , cupbabycommon : List Choosing.Model
--     , cupthumbcommon : List Choosing.Model
--     , cupindexcommon : List Choosing.Model
--     , anglethumbcommon : List Choosing.Model
--     , anglebabycommon : List Choosing.Model
--     , flatthumbcommon : List Choosing.Model
--     , flatbabycommon : List Choosing.Model
--     }
-- convertGroupHandChoosings : HandGroupImportModel -> HandGroupModel
-- convertGroupHandChoosings handgroupchoosings =
--     { fistbabycommon = List.map (Choosing.toModel 0) handgroupchoosings.fistbabycommon
--     , fistringcommon = List.map (Choosing.toModel 0) handgroupchoosings.fistringcommon
--     , fistmiddlecommon = List.map (Choosing.toModel 0) handgroupchoosings.fistmiddlecommon
--     , fistindexcommon = List.map (Choosing.toModel 0) handgroupchoosings.fistindexcommon
--     , fistthumbcommon = List.map (Choosing.toModel 0) handgroupchoosings.fistthumbcommon
--     , circlethumbcommon = List.map (Choosing.toModel 0) handgroupchoosings.circlethumbcommon
--     , circleindexcommon = List.map (Choosing.toModel 0) handgroupchoosings.circleindexcommon
--     , circleringcommon = List.map (Choosing.toModel 0) handgroupchoosings.circleringcommon
--     , circlebabycommon = List.map (Choosing.toModel 0) handgroupchoosings.circlebabycommon
--     , cupbabycommon = List.map (Choosing.toModel 0) handgroupchoosings.cupbabycommon
--     , cupthumbcommon = List.map (Choosing.toModel 0) handgroupchoosings.cupthumbcommon
--     , cupindexcommon = List.map (Choosing.toModel 0) handgroupchoosings.cupindexcommon
--     , anglethumbcommon = List.map (Choosing.toModel 0) handgroupchoosings.anglethumbcommon
--     , anglebabycommon = List.map (Choosing.toModel 0) handgroupchoosings.anglebabycommon
--     , flatthumbcommon = List.map (Choosing.toModel 0) handgroupchoosings.flatthumbcommon
--     , flatbabycommon = List.map (Choosing.toModel 0) handgroupchoosings.flatbabycommon
--     }


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



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
