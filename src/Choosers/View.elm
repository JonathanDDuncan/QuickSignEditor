module Choosers.View exposing (root)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style, class)
import Choosers.Types exposing (Model, Msg(Editor, SignView), ChoosingModel)
import Choosers.Types as Editor exposing (Editor)
import Helpers.ViewExtra exposing (px, (=>), calculatescale, transformscale)
import Choosers.HandGroupChooser exposing (handgroupchooser, createhandgroupchooserdata)
import Choosers.GeneralGroupChooser exposing (generalgroupchooser, creategeneralgroupchooserdata)
import Choosers.GeneralSymbolChooser exposing (getgeneralsymbolchooser, generalsymbolchooser)
import Choosers.HandSymbolChooser exposing (handsymbolchooser)
import SW.Symbol exposing (Symbol, symbolinit, iskey)
import Choosers.Maniquin exposing (..)


root : Int -> Int -> Choosers.Types.Model -> Html Choosers.Types.Msg
root parentwidth parentheight model =
    let
        halfheight =
            Basics.truncate (Basics.toFloat parentheight / Basics.toFloat 2)

        halfwidth =
            Basics.truncate (Basics.toFloat parentwidth / Basics.toFloat 2)

        maniqin1 =
            maniquin model halfwidth halfheight

        symbolchooserheight =
            parentheight - maniqin1.height

        symbolchooser =
            getsymbolchooser model halfwidth

        symbolchooserscale =
            calculatescale (Basics.toFloat symbolchooser.width)
                (Basics.toFloat symbolchooser.height)
                (Basics.toFloat halfwidth)
                (Basics.toFloat (parentheight - maniqin1.divheight))
    in
        div []
            [ maniqin1.view
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


getsymbolchooser : Choosers.Types.Model -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
getsymbolchooser model halfwidth =
    let
        chooser =
            if model.groupselected.symbolkey == "" then
                { display = text "", width = 1, height = 1 }
            else if iskey model.groupselected.symbolkey "hand" then
                handsymbolchooser model halfwidth
            else
                gensymbolchooser model halfwidth
    in
        chooser


gensymbolchooser : Choosers.Types.Model -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
gensymbolchooser model halfwidth =
    let
        generalsymbolchooserdata =
            getgeneralsymbolchooser model.groupselected model.symbolsizes model.selectedcolumn
    in
        generalsymbolchooser model.groupselected halfwidth generalsymbolchooserdata


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
