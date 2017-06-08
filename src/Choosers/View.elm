module Choosers.View exposing (root, choosingroot)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick, onMouseDown, onDoubleClick)
import Choosers.Types exposing (Model, Msg(Editor, SignView), ChoosingModel)
import Choosers.Types as Editor exposing (Editor)
import Helpers.ViewExtra exposing (px, (=>), calculatescale, transformscale)
import Choosers.HandGroupChooser exposing (handgroupchooser, createhandgroupchooserdata)
import Choosers.GeneralGroupChooser exposing (generalgroupchooser, creategeneralgroupchooserdata)
import Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser, generalsymbolchooser)
import Choosers.HandSymbolChooser exposing (handsymbolchooser)
import SW.Types exposing (Symbol, Sign, symbolinit, iskey)
import SWEditor.DisplaySvg


root : Int -> Int -> Choosers.Types.Model -> Html Choosers.Types.Msg
root parentwidth parentheight model =
    let
        halfheight =
            Basics.truncate (Basics.toFloat parentheight / Basics.toFloat 2)

        halfwidth =
            Basics.truncate (Basics.toFloat parentwidth / Basics.toFloat 2)

        maniquin =
            choosingroot model halfwidth 10

        maniquinscale =
            calculatescale (Basics.toFloat maniquin.width)
                (Basics.toFloat maniquin.height)
                (Basics.toFloat halfwidth)
                (Basics.toFloat halfheight)

        maniquindivheight =
            truncate <|
                Basics.toFloat maniquin.height
                    * maniquinscale

        symbolchooserheight =
            parentheight - maniquin.height

        symbolchooser =
            getsymbolchooser model halfwidth symbolchooserheight

        symbolchooserscale =
            calculatescale (Basics.toFloat symbolchooser.width)
                (Basics.toFloat symbolchooser.height)
                (Basics.toFloat halfwidth)
                (Basics.toFloat (parentheight - maniquindivheight))
    in
        div []
            [ div
                [ style
                    [ ( "transform-origin", "top left" )
                    , transformscale maniquinscale
                    , "height" => px maniquindivheight
                    , "width" => px halfwidth
                      -- , "margin-top" => px -10
                    ]
                ]
                [ maniquin.display ]
            , div
                [ class "generalsymbolchooser"
                , style
                    [ "height" => px halfheight
                    , "display" => "inline-block"
                    , "margin-top" => "5px"
                    , "float" => "left"
                    ]
                ]
                [ div
                    [ style
                        [ "position" => "relative"
                        , ( "transform-origin", "top left" )
                        , transformscale symbolchooserscale
                        , "height" => px symbolchooserheight
                        , "width" => px halfwidth
                        ]
                    ]
                    [ symbolchooser.display ]
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
                ]
                [ choosesubgroupchooser model
                ]
            ]


choosingroot : Choosers.Types.Model -> Int -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
choosingroot model height bottompadding =
    let
        size =
            List.foldr
                (\choosing accumulator ->
                    let
                        newright =
                            Basics.max accumulator.right (choosing.offset.offsetx + choosing.displaySign.width)

                        newleft =
                            Basics.min accumulator.left choosing.offset.offsetx

                        newbottom =
                            Basics.max accumulator.bottom (choosing.offset.offsety + choosing.displaySign.height)

                        newtop =
                            Basics.min accumulator.top choosing.offset.offsety
                    in
                        { top = newtop, bottom = newbottom, right = newright, left = newleft }
                )
                { top = 0, bottom = 0, right = 0, left = 0 }
                model.choosings
    in
        { display =
            div
                [ style [ "position" => "relative" ]
                ]
                (List.map displayChoosing model.choosings)
        , width = size.right - size.left
        , height = size.bottom - size.top + bottompadding
        }


getsymbolchooser : Choosers.Types.Model -> Int -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
getsymbolchooser model halfwidth halfheight =
    let
        chooser =
            if model.groupselected.symbolkey == "" then
                { display = text "", width = 1, height = 1 }
            else if iskey model.groupselected.symbolkey "hand" then
                handsymbolchooser model halfwidth
            else
                gensymbolchooser model halfwidth halfheight
    in
        chooser


gensymbolchooser : Choosers.Types.Model -> Int -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
gensymbolchooser model halfwidth halfheight =
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn
    in
        generalsymbolchooser model.groupselected halfwidth generalsymbolchooserdata


displayChoosing : ChoosingModel -> Html Choosers.Types.Msg
displayChoosing choosing =
    div
        [ onClick ((Editor << Editor.Clicked) choosing.value)
        , onMouseDown ((Editor << Editor.DragSymbol) (firstsymbol choosing).key)
        , onDoubleClick
            ((Editor << Editor.ReplaceSymbol) (firstsymbol choosing).key)
        ]
        [ Html.map SignView
            (SWEditor.DisplaySvg.signdisplaysvgposition "hover" choosing.displaySign choosing.offset.offsetx choosing.offset.offsety)
        ]


firstsymbol : { b | valuestoAdd : List Symbol } -> Symbol
firstsymbol choosing =
    choosing.valuestoAdd |> List.head |> Maybe.withDefault symbolinit


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
                generalgroupchooser <| creategeneralgroupchooserdata model
