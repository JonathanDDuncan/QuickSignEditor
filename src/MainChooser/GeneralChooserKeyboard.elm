module MainChooser.GeneralChooserKeyboard exposing (..)

import MainChooser.Types exposing (..)
import Html
import Choosing.View exposing (root)
import Choosing.Types exposing (Model)
import Keyboard.Shared exposing (..)


creategeneralchooserkeyboard :
    List Choosing.Types.Model
    -> List (KeyAction Msg)
creategeneralchooserkeyboard choosings =
    let
        keyboardlayout =
            layout
    in
        List.indexedMap
            (\i choosing ->
                { test = { key = getkey i keyboardlayout, ctrl = False, shift = False, alt = False }
                , action = (Noop)
                , display =
                    { width = choosing.displaySign.width
                    , height = choosing.displaySign.height
                    , view = Html.map Choosing (Choosing.View.normal <| Debug.log "choosing" choosing)
                    }
                }
            )
            (choosings)


getkey : Int -> List { index : Int, key : Int } -> Int
getkey n layout =
    List.filter (\l -> l.index == n) layout
        |> List.head
        |> Maybe.withDefault { index = 0, key = 0 }
        |> .key


layout : List { index : Int, key : Int }
layout =
    [ { index = 0, key = 2 }
    , { index = 1, key = 3 }
    , { index = 2, key = 4 }
    , { index = 3, key = 0 }
    , { index = 4, key = 5 }
    , { index = 5, key = 7 }
    , { index = 6, key = 6 }
    , { index = 7, key = 35 }
    , { index = 8, key = 8 }
    , { index = 9, key = 36 }
    , { index = 10, key = 37 }
    , { index = 11, key = 0 }
    , { index = 12, key = 34 }
    , { index = 13, key = 20 }
    , { index = 14, key = 21 }
    , { index = 15, key = 43 }
    , { index = 16, key = 9 }
    , { index = 17, key = 16 }
    , { index = 18, key = 46 }
    , { index = 19, key = 17 }
    , { index = 20, key = 18 }
    , { index = 21, key = 19 }
    , { index = 22, key = 30 }
    , { index = 23, key = 31 }
    , { index = 24, key = 33 }
    , { index = 25, key = 47 }
    , { index = 26, key = 44 }
    , { index = 27, key = 10 }
    , { index = 28, key = 48 }
    , { index = 29, key = 49 }
    , { index = 30, key = 50 }
    , { index = 31, key = 45 }
    , { index = 32, key = 32 }
    , { index = 33, key = 11 }
    ]
