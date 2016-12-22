module SW.State exposing (signinit, symbolinit, iskey)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)
import ParseInt as ParseInt exposing (..)
import String exposing (..)

signinit : EditorSign
signinit =
    { width = 0
    , height = 0
    , text = ""
    , x = 0
    , y = 0
    , backfill = ""
    , syms = [ symbolinit ]
    , laned = False
    , left = 0
    }


symbolinit : EditorSymbol
symbolinit =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , fontsize = 0
    , size = 1
    , nwcolor = ""
    , pua = ""
    , code = 0
    , key = ""
    , nbcolor = ""
    , selected = False
    , id = 0
    }

type alias  TypeRange  = {
    start : String
    , end : String
}

typerange: String -> TypeRange
typerange typename = 
     case typename of 
        "writing" -> 
            { start ="100"
            , end ="37e"
            }
        
        "hand"-> 
            { start = "100"
            , end ="204"
            }
      
        "movement"-> 
            { start ="205"
            , end ="2f6"
            }
                  
        "dynamic"-> 
            { start ="2f7"
            , end ="2fe"
            }
              
        "head" -> 
            { start ="2ff"
            , end ="36c"
            }  
        "hcenter"-> 
            { start ="2ff"
            , end ="36c"
            }
      
        "vcenter"-> 
            { start ="2ff"
            , end ="375"
            }
        
        "trunk"-> 
            { start ="36d"
            , end ="375"
            }
       
        "limb"-> 
            { start ="376"
            , end ="37e"
            }
        
        "location"-> 
            { start ="37f"
            , end ="386"
            }
            
        "punctuation"-> 
            { start ="387"
            , end ="38b"
            }
        
        _ -> 
         { start ="100"
         , end ="38b"
         }
      
--typerange "hand" -> { start = "100", end = "204" } : { start : String, end : String }

iskey: String -> String -> Bool
iskey key typename = 
    let range = typerange typename
        start = ParseInt.parseIntHex  range.start 
        |> Result.toMaybe
         |> Maybe.withDefault 0
        end = ParseInt.parseIntHex   range.end
         |> Result.toMaybe 
         |> Maybe.withDefault 0
        char = String.slice  1 4 key 
         |> ParseInt.parseIntHex 
          |> Result.toMaybe |> Maybe.withDefault 0
   in 
       start <= char && end >= char
         
 --iskey "S100" "hand" -> True
 --iskey "S100" "head" -> False

-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
