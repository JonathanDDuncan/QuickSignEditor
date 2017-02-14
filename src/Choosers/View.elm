module Choosers.View exposing (root)

import Html exposing (..)
import String exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Choosers.Types exposing (..)
import Choosing.View exposing (..)
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


root : Choosers.Types.Model -> Int -> Int -> Html Choosers.Types.Msg
root model parentwidth parentheight =
    let
        halfheight =
            (Basics.truncate ((Basics.toFloat parentheight) / Basics.toFloat 2))

        halfwidth =
            (Basics.truncate ((Basics.toFloat parentwidth) / Basics.toFloat 2))
    in
        div []
            [ div
                [ style [ "height" => px (halfheight - 30) ]
                , onMouseEnter (SetKeyboardMode Keyboard.Shared.GeneralChooser)
                , style [ "position" => "relative" ]
                ]
                (List.map displayChoosing <| Debug.log "modelchoosings" model.choosings)
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


symbolchooser : Choosers.Types.Model -> Int -> Int -> Html Choosers.Types.Msg
symbolchooser model halfwidth halfheight =
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn
    in
        if iskey model.groupselected.symbolkey "hand" then
            handsymbolchooser model halfwidth halfheight
        else
            generalsymbolchooser model.groupselected halfwidth halfheight generalsymbolchooserdata


displayChoosing : Choosing.Types.Model -> Html Choosers.Types.Msg
displayChoosing choosing =
    div
        [ onClick (Clicked choosing.value)
        , onMouseDown (DragSymbol (firstsymbol choosing).code)
        , onDoubleClick
            (ReplaceSymbol (firstsymbol choosing).code)
        ]
        [ Html.map Choosing (Choosing.View.root choosing) ]


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



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
