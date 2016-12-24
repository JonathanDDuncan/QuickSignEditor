module MainChooser.Types exposing (..)

-- import SubMainChoosers.Types

import Choosing.Types as Choosing
import SWEditor.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
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
    , handsymbol : HandSymbol
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
    | UpdateHandSymbolChooser


type alias HandGroupImportModel =
    { chooseritemvalues : List ChooserItemValue
    , basechooseritems : List BaseChooserItem
    , symbolsizes : List SymbolSize
    }


type alias HandSymbol =
    { hand : Hands
    , plane : Planes
    , handfill : HandFills
    , rotationselection : Int
    , flowersymbols : Flower
    , symbollefthand : EditorSymbol
    , symbolrighthand : EditorSymbol
    , handfillitems : List HandFillItem
    }


type HandFills
    = LeftBack
    | LeftThumbEdge
    | LeftPalm
    | LeftBabyEdge
    | RightBack
    | RightThumbEdge
    | RightPalm
    | RightBabyEdge


handsymbolinit =
    { hand = Right
    , plane = Wall
    , handfill = RightPalm
    , rotationselection = 1
    , flowersymbols = flowerinit
    , symbollefthand = symbolinit
    , symbolrighthand = symbolinit
    , handfillitems = []
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


type alias HandFillItem =
    { fill : Fill
    , filltype : HandFills
    , planetype : Planes
    , rotation : Int
    , symbol : EditorSymbol
    }


getchoosings : String -> List { b | basesymbol : String, choosings : List a } -> List a
getchoosings basesymbol allgroupchoosings =
    let
        firstfound =
            List.head <| List.filter (\agc -> agc.basesymbol == basesymbol) allgroupchoosings

        choosings =
            case firstfound of
                Just groupchoosings ->
                    groupchoosings.choosings

                Nothing ->
                    []
    in
        choosings


type alias Petal =
    { fill : Fill
    , filltype : HandFills
    , planetype : Planes
    , rotation : Int
    , rotationoffset : Int
    , symbol : EditorSymbol
    }


petalinit =
    { fill = 1
    , filltype = LeftBack
    , planetype = Wall
    , rotation = 1
    , rotationoffset = 0
    , symbol = symbolinit
    }


type alias Flower =
    { handfill1 : Petal
    , handfill2 : Petal
    , handfill3 : Petal
    , handfill4 : Petal
    , handfill5 : Petal
    , handfill6 : Petal
    , handfill7 : Petal
    , handfill8 : Petal
    }


flowerinit =
    { handfill1 = petalinit
    , handfill2 = petalinit
    , handfill3 = petalinit
    , handfill4 = petalinit
    , handfill5 = petalinit
    , handfill6 = petalinit
    , handfill7 = petalinit
    , handfill8 = petalinit
    }



-- Plus any other types unique to this feature
-- Plus any library function to run on the types
