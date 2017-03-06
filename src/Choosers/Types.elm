module Choosers.Types exposing (..)

-- import SubMainChoosers.Types

import SWEditor.Types exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.EditorSign exposing (..)
import SW.Types exposing (..)
import Dict exposing (..)
import Material exposing (..)
import Keyboard.Shared exposing (..)


type alias Model =
    { lastmdlid : Int
    , mdl : Material.Model
    , choosings : List ChoosingModel
    , handgroupchoosings : HandGroupModel
    , allgroupchoosings : AllGroupChoosings
    , clicked : String
    , selectedcolumn : Int
    , groupselected : ChooserItem
    , handgroupfilter : Int
    , symbolsizes : Dict.Dict String Size
    , handsymbol : HandSymbol
    , handgroupchooseritems : List HandGroupChooser
    , generalgroupchooserdata : List (List GeneralGroupChooserColumData)
    , chooserskeyboard : ChoosersKeyboard
    }


type alias ChoosersKeyboard =
    { generalchooserkeyboard : List (KeyAction Msg)
    , groupchooserkeyboard : List (KeyAction Msg)
    , symbolchooserkeyboard : List (KeyAction Msg)
    , keyboardpage : Int
    }


type Msg
    = Noop
    | MainChooserMessage
    | RequestInitialChoosings
    | ReceiveInitialChoosings (List ChoosingImportModel)
    | ReceiveInitialGroupHandChoosings HandGroupImportModel
    | Clicked String
    | SymbolView SWEditor.Types.Msg
    | SignView SWEditor.Types.Msg
    | SelectedColumn Int
    | GroupSelected ChooserItem
    | AddSymbol Symbol
    | DragSymbol Key
    | ReplaceSymbol Key
    | FilterHandGroup Int
    | Mdl (Material.Msg Msg)
    | SelectHand Hands
    | SelectPlane Planes
    | SelectHandFill HandFills
    | UpdateHandSymbolChooser
    | Keyboard KeyboardCommand
    | NextKeyboardPage
    | SetKeyboardMode KeyboardMode
    | UpdateChooserKeyboards


type alias HandGroupChooser =
    List (List HandGroupChooserSubList)


type alias HandGroupChooserSubList =
    { backgroundcolor : String
    , displayhanditems :
        List
            { chooseritem : ChooserItem
            , mdlid : Int
            , symbol : Symbol
            }
    }


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
    , flowersymbols : List Petal
    , symbollefthand : Symbol
    , symbolrighthand : Symbol
    , handfillitems : List HandFillItem
    }


handsymbolinit : HandSymbol
handsymbolinit =
    { hand = Right
    , plane = Wall
    , handfill = RightPalm
    , rotationselection = 1
    , flowersymbols = []
    , symbollefthand = symbolinit
    , symbolrighthand = symbolinit
    , handfillitems = []
    }


gethandtype : HandFills -> Hands
gethandtype filltype =
    case filltype of
        LeftBack ->
            Left

        LeftThumbEdge ->
            Left

        LeftPalm ->
            Left

        LeftBabyEdge ->
            Left

        RightBack ->
            Right

        RightThumbEdge ->
            Right

        RightPalm ->
            Right

        RightBabyEdge ->
            Right


type HandFills
    = LeftBack
    | LeftThumbEdge
    | LeftPalm
    | LeftBabyEdge
    | RightBack
    | RightThumbEdge
    | RightPalm
    | RightBabyEdge


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
    , feature : String
    , rowname : String
    , colname : String
    , rank : Int
    }


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
    , symbol : Symbol
    }


type alias HandItem =
    { fill : Fill
    , filltype : HandFills
    , planetype : Planes
    , rotation : Int
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


type alias HandPng =
    { miror : Bool
    , pngcss : String
    , rotate : Int
    }


type alias Petal =
    { fill : Fill
    , filltype : HandFills
    , planetype : Planes
    , rotation : Int
    , rotationoffset : Int
    , symbol : Symbol
    , handpng : HandPng
    }


petalinit : Petal
petalinit =
    { fill = 1
    , filltype = LeftBack
    , planetype = Wall
    , rotation = 1
    , rotationoffset = 0
    , symbol = symbolinit
    , handpng =
        { miror = False
        , pngcss = ""
        , rotate = 0
        }
    }



-- Plus any other types unique to this feature
-- Plus any library function to run on the types


bkcolor : number -> number_ -> String
bkcolor cat col =
    case cat of
        1 ->
            "#ff9999"

        2 ->
            "#ffc799"

        3 ->
            "#ffee99"

        4 ->
            "#a0f8a0"

        _ ->
            "#a8bcf0"


type alias GeneralGroupChooserColumData =
    { col : Int, row : Int, symboldatalist : List GeneralGroupChooserSymbolData }


type alias GeneralGroupChooserSymbolData =
    { chooseritem : ChooserItem
    , mdlid : Int
    , modelmdl : Material.Model
    , symbol : Symbol
    }


type alias HandGroupChooserViewColumnData =
    { backgroundcolor : String
    , symboldatalist : List HandGroupChooserViewSymbolData
    , col : Int
    , row : Int
    }


type alias HandGroupChooserViewSymbolData =
    { chooseritem : ChooserItem
    , mdlid : Int
    , modelmdl : Material.Model
    , symbol : Symbol
    }



-- Choosing


type alias ChoosingModel =
    { displaySign : Sign
    , valuestoAdd : List Symbol
    , value : String
    , offset : Offset
    }


type alias ChoosingImportModel =
    { displaySign : PortableSign
    , valuestoAdd : List Symbol
    , value : String
    , offset : Offset
    }



-- Plus any other types unique to this feature
-- Plus any library function to run on the types


toModel : Int -> ChoosingImportModel -> ChoosingModel
toModel id importmodel =
    let
        sign =
            SWEditor.EditorSign.updateSymbolIds (portableSigntoSign importmodel.displaySign) id

        symbols =
            List.indexedMap (SWEditor.EditorSymbol.updateId id)
                importmodel.valuestoAdd
    in
        { displaySign =
            sign
        , valuestoAdd = symbols
        , value = importmodel.value
        , offset = importmodel.offset
        }
