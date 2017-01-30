module MainChooser.GroupChooserKeyboard exposing (..)

import MainChooser.Types exposing (..)
import Html
import Keyboard.Shared exposing (..)
import MainChooser.GeneralGroupChooser exposing (..)


creategroupchooserkeyboard : Model -> List (KeyAction Msg)
creategroupchooserkeyboard model =
    let
        basesymbol =
            String.slice 0 4 model.clicked
    in
        case basesymbol of
            "S14c" ->
                handgroupchooserkeyboard model

            _ ->
                generalgroupchooserkeyboard model


generalgroupchooserkeyboard model =
    let
        generalgroupchooserdata =
            creategeneralgroupchooserdata model

        mylist =
            []
    in
        List.indexedMap
            (\i item ->
                let
                    -- layoutsetting =
                    --     getlayoutsettings i layout
                    -- displayheight =
                    --     case layoutsetting.overrideheight of
                    --         Just value ->
                    --             value
                    --         Nothing ->
                    --             choosing.displaySign.height
                    a =
                        5

                    key =
                        0

                    action =
                        (Noop)

                    itemwidth =
                        20

                    itemheight =
                        20

                    symbolview =
                        Html.text "view"
                in
                    { test = { key = key, ctrl = False, shift = False, alt = False }
                    , action = action
                    , display =
                        { width = itemwidth
                        , height =
                            itemheight
                        , view = Html.map Choosing (symbolview)
                        }
                    }
            )
            (mylist)


handgroupchooserkeyboard model =
    []
