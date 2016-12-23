module MainChooser.Types exposing (..)

-- import SubMainChoosers.Types

import Choosing.Types as Choosing
import SWEditor.Types exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import Material exposing (..)


type alias Model =
    { lastmdlid : Int
    , mdl : Material.Model
    , choosings : List Choosing.Model
    , handgroupchoosings : HandGroupModel
    , allgroupchoosings : AllGroupChoosings
    , clicked : String
    , selectedcolumn : Int
    , groupselected : ChooserItem
    , handgroupfilter : Int
    , symbolsizes : Dict.Dict String Size
    , handsymbol: HandSymbol
    }


type Msg
    = Noop
    | MainChooserMessage
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
    | Mdl (Material.Msg Msg)
    | SelectHand Hands
    | SelectPlane Planes
    | SelectHandFill HandFills

type alias HandGroupImportModel =
    { chooseritemvalues : List ChooserItemValue
    , basechooseritems : List BaseChooserItem
    , symbolsizes : List SymbolSize
    }

type alias HandSymbol =
    { hand: Hands
    , plane: Planes
    , handfill: HandFills
    , rotationselection : Int
    }


type HandFills =
    LeftBack
    | LeftThumbEdge
    | LeftPalm
    | LeftBabyEdge
    | RightBack  
    | RightThumbEdge
    | RightPalm
    | RightBabyEdge

handsymbolinit :
    { hand : Hands
    , handfill : HandFills
    , plane : Planes
    , rotationselection : number
    }
handsymbolinit =
    { hand = Right
    , plane = Wall
    , handfill =   RightPalm 
    , rotationselection = 1
    }
type alias SymbolSize =
    { k : String
    , h : Int
    , w : Int
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
    , thumb : Bool
    , rowname : String
    , colname : String
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
    , thumb : Bool
    , row : Int
    , col : Int
    , rank : Int
    }


chooseriteminit :
    { base : number
    , common : Bool
    , thumb : Bool
    , groupchooser : Int
    , name : String
    , col : Int
    , rank : Int
    , row : Int
    , symbolid : String
    , symbolkey : String
    , unicodepua : String
    , validfills : String
    , validrotations : String
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
    , thumb = False
    , row = 1
    , col = 1
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


type Hands 
    = Right
    | Left

type Planes 
    = Wall
    | Floor
-- Plus any other types unique to this feature
-- Plus any library function to run on the types
