module Choosing.Types exposing (..)

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Types exposing (..)
import SW.Types exposing (..)


type alias ChoosingModel =
    { displaySign : SWEditor.EditorSign.EditorSign
    , valuestoAdd : List SWEditor.EditorSymbol.EditorSymbol
    , value : String
    , offset : Offset
    }


type alias ChoosingImportModel =
    { displaySign : Sign
    , valuestoAdd : List Symbol
    , value : String
    , offset : Offset
    }


type ChoosingMsg
    = ChoosingMessage
    | Display SWEditor.Types.Msg



-- Plus any other types unique to this feature
-- Plus any library function to run on the types


toModel : Int -> ChoosingImportModel -> ChoosingModel
toModel id importmodel =
    let
        sign =
            SWEditor.EditorSign.toEditorSign importmodel.displaySign id

        symbols =
            List.indexedMap (SWEditor.EditorSymbol.toEditorSymbol id)
                importmodel.valuestoAdd
    in
        { displaySign =
            sign
        , valuestoAdd = symbols
        , value = importmodel.value
        , offset = importmodel.offset
        }
