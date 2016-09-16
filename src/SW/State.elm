module SW.State exposing (signinit, symbolinit)

-- only includes Rest functions that are really needed
-- import Rest exposing (..)
-- import Ports exposing (..)
-- if you have sub components
-- import PlatformHelpers exposing (..)

import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)


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
    , nwcolor = ""
    , pua = ""
    , code = 0
    , key = ""
    , nbcolor = ""
    , selected = False
    , id = 0
    }



-- To nest subscriptions
-- Sub.batch
--       [ SubSWEditor.State.subscriptions model.subSWEditorFieldName |> Sub.map SubSWEditorMsg
--       ]
