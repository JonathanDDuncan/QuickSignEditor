module Choosers.Types
    exposing
        ( Model
        , Msg(..)
        , ChoosingModel
        , GroupChoosing
        , ChoosersKeyboard
        , HandSymbol
        , HandPng
        , HandGroupChooserSubList
        , HandItem
        , HandFillItem
        , Petal
        , gethandtype
        , bkcolor
        , getchoosings
        , handsymbolinit
        , ishandgroupchooser
        , Update
        )

-- import SubMainChoosers.Types

import SWEditor.Types exposing (Model, Msg, Offset)
import SW.Types exposing (Size)
import SW.Sign exposing (Sign)
import SW.Pua exposing (Fill, Base, Key)
import SW.Symbol exposing (Symbol, symbolinit)
import Material exposing (Model)
import Dict exposing (Dict)
import Keyboard.Shared exposing (KeyAction, KeyboardCommand)
import Choosers.KeyboardType exposing (KeyboardType)
import Choosers.ChooserItemType exposing (ChooserItem)
import Choosers.EditorType exposing (Editor)
import Choosers.LoadingType exposing (Loading)
import SW.HandFillsType exposing (HandFills(..))
import SW.HandsType exposing (Hands(..))
import SW.PlanesType exposing (Planes(..))


type alias Update =
    Msg -> Model -> ( Model, Cmd Msg )


type alias Model =
    { lastmdlid : Int
    , mdl : Material.Model
    , maniquinchoosings : List ChoosingModel
    , groupchoosings : List GroupChoosing
    , clicked : String
    , selectedcolumn : Int
    , groupselected : ChooserItem
    , handgroupfilter : Int
    , symbolsizes : Dict.Dict String Size
    , handsymbol : HandSymbol
    , chooserskeyboard : ChoosersKeyboard
    }


type alias ChoosersKeyboard =
    { maniquinkeyboard : List (KeyAction Msg)
    , groupchooserkeyboard : List (KeyAction Msg)
    , symbolchooserkeyboard : List (KeyAction Msg)
    , keyboardpage : Int
    }


type Msg
    = Noop
    | Mdl (Material.Msg Msg)
    | FilterHandGroup Int
    | SelectHand Hands
    | SelectPlane Planes
    | SelectHandFill HandFills
    | UpdateHandSymbolChooser
    | EditorMsg Editor
    | KeyboardMsg KeyboardType
    | LoadingMsg Loading


type alias HandGroupChooserSubList =
    { backgroundcolor : String
    , displayhanditems :
        List
            { chooseritem : ChooserItem
            , mdlid : Int
            , symbol : Symbol
            }
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


type alias GroupChoosing =
    { basesymbol : String
    , choosings : List ChooserItem
    }


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


getchoosings : List { b | basesymbol : String, choosings : List a } -> String -> List a
getchoosings groupchoosings basesymbol =
    let
        firstfound =
            List.head <| List.filter (\agc -> agc.basesymbol == basesymbol) groupchoosings

        choosings =
            case firstfound of
                Just groupchoosings1 ->
                    groupchoosings1.choosings

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



-- Plus any other types unique to this feature
-- Plus any library function to run on the types


bkcolor : number -> String
bkcolor cat =
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



-- Choosing


type alias ChoosingModel =
    { displaySign : Sign
    , valuestoAdd : List Symbol
    , value : String
    , offset : Offset
    }



-- Plus any other types unique to this feature
-- Plus any library function to run on the types


ishandgroupchooser : String -> Bool
ishandgroupchooser clicked =
    let
        basesymbol =
            String.slice 0 4 clicked
    in
        case basesymbol of
            "S14c" ->
                True

            _ ->
                False
