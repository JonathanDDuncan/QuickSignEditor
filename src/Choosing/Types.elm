module Choosing.Types exposing (..)

-- import SubChoosings.Types

import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SWEditor.Types exposing (..)
import SW.Types exposing (..)


type alias Model =
    { displaySign : SWEditor.EditorSign.EditorSign
    , valuestoAdd : List SWEditor.EditorSymbol.EditorSymbol
    , value : Int
    , offset : Offset
    }


type alias ImportModel =
    { displaySign :
        Sign
        -- , valuestoAdd : List Symbol
        -- , value : Int
        -- , offset : Offset
    }


type Msg
    = ChoosingMessage



-- Plus any other types unique to this feature
-- Plus any library function to run on the types


toModel : Int -> ImportModel -> Model
toModel id importmodel =
    let
        sign =
            SWEditor.EditorSign.toEditorSign importmodel.displaySign id

        -- symbols =
        --     List.indexedMap (SWEditor.EditorSymbol.toEditorSymbol id)
        --         importmodel.valuestoAdd
    in
        { displaySign =
            sign
            -- , valuestoAdd = symbols
            -- , value = importmodel.value
            -- , offset = importmodel.offset
        , valuestoAdd = []
        , value = 101
        , offset = Offset 0 1
        }
