module Choosers.View exposing (root, choosingroot)

import Html exposing (..)
import String exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Choosers.Types exposing (..)
import Choosing.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Choosers.HandGroupChooser exposing (..)
import Choosers.GeneralGroupChooser exposing (..)
import Choosers.GeneralSymbolChooser exposing (..)
import Choosers.HandSymbolChooser exposing (..)
import SW.Types exposing (iskey)
import SWEditor.EditorSymbol exposing (..)
import SW.Types exposing (..)
import Keyboard.Shared exposing (KeyboardMode)
import SWEditor.DisplaySvg exposing (..)


root : Int -> Int -> Choosers.Types.Model -> Html Choosers.Types.Msg
root parentwidth parentheight model =
    let
        halfheight =
            (Basics.truncate ((Basics.toFloat parentheight) / Basics.toFloat 2))

        halfwidth =
            (Basics.truncate ((Basics.toFloat parentwidth) / Basics.toFloat 2))
    in
        div []
            [ choosingroot halfheight model
            , div
                [ class "generalsymbolchooser"
                , style
                    [ "height" => px halfheight
                    , "display" => "inline-block"
                    , "margin-top" => "5px"
                    , "float" => "left"
                    ]
                , onMouseEnter (SetKeyboardMode Keyboard.Shared.SymbolChooser)
                ]
                [ symbolchooser model halfwidth halfheight
                ]
            , div
                [ style
                    [ "position" => "absolute"
                    , "width" => "50%"
                    , "left" => "0px"
                    , "top" => "0px"
                    , "margin-left" => "50%"
                    , "height" => px parentheight
                    , "margin-top" => "5px"
                    , "overflow-y" => "scroll"
                    , "overflow-x" => "scroll"
                    ]
                , onMouseEnter (SetKeyboardMode Keyboard.Shared.GroupChooser)
                ]
                [ choosesubgroupchooser model
                ]
            ]


choosingroot : Int -> Choosers.Types.Model -> Html Choosers.Types.Msg
choosingroot height model =
    div
        [ style [ "height" => px (height - 30) ]
        , onMouseEnter (SetKeyboardMode Keyboard.Shared.GeneralChooser)
        , style [ "position" => "relative" ]
        ]
        (List.map displayChoosing model.choosings)


symbolchooser : Choosers.Types.Model -> Int -> Int -> Html Choosers.Types.Msg
symbolchooser model halfwidth halfheight =
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn
    in
        if model.groupselected.symbolkey == "" then
            text ""
        else if iskey model.groupselected.symbolkey "hand" then
            handsymbolchooser model halfwidth halfheight
        else
            generalsymbolchooser model.groupselected halfwidth halfheight generalsymbolchooserdata


displayChoosing : Choosing.Types.Model -> Html Choosers.Types.Msg
displayChoosing choosing =
    div
        [ onClick (Clicked choosing.value)
        , onMouseDown (DragSymbol (firstsymbol choosing).key)
        , onDoubleClick
            (ReplaceSymbol (firstsymbol choosing).key)
        ]
        [ Html.map SignView
            (SWEditor.DisplaySvg.signdisplaysvgposition choosing.displaySign choosing.offset.offsetx choosing.offset.offsety)
        ]


firstsymbol : { b | valuestoAdd : List EditorSymbol } -> EditorSymbol
firstsymbol choosing =
    choosing.valuestoAdd |> List.head |> Maybe.withDefault SWEditor.EditorSymbol.symbolinit


choosesubgroupchooser : Choosers.Types.Model -> Html Choosers.Types.Msg
choosesubgroupchooser model =
    let
        basesymbol =
            String.slice 0 4 model.clicked
    in
        case basesymbol of
            "S14c" ->
                handgroupchooser <| createhandgroupchooserdata model

            _ ->
                generalgroupchooser model.generalgroupchooserdata
