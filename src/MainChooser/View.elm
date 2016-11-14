module MainChooser.View exposing (root)

import Html exposing (..)
import String exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import Choosing.View exposing (..)
import Choosing.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import MainChooser.HandGroupChooserView exposing (..)
import MainChooser.GeneralGroupChooserView exposing (..)
import MainChooser.GeneralSymbolChooserView exposing (..)


--import SubMainChooser.View exposing (root)


root : MainChooser.Types.Model ->  Int -> Int -> Html MainChooser.Types.Msg
root model parentwidth parentheight =
    let
        halfheight =
            (Basics.truncate ((Basics.toFloat parentheight) / Basics.toFloat 2))
        halfwidth =  (Basics.truncate ((Basics.toFloat parentwidth) / Basics.toFloat 2))
    in
        div []
            [ div
                [ style [ "height" => px (halfheight - 40 ) ] ]
                (List.map displayChoosing model.choosings)
            , div
                [ class "generalsymbolchooser", style [ "height" => px halfheight, "display" => "inline-block", "margin-top" => "5px", "float" => "left" ] ]
                [ generalsymbolchooser model.groupselected model.selectedcolumn model.symbolsizes halfwidth halfheight
                ]
            , div
                [ style [ "position" => "absolute", "width" => "50%", "left" => "0px", "top" => "0px", "margin-left" => "50%", "height" => px parentheight, "margin-top" => "5px", "overflow-y" => "scroll", "overflow-x" => "scroll" ] ]
                [ choosesubgroupchooser model
                ]
            ]


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


choosesubgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
choosesubgroupchooser model =
    let
        basesymbol =
            String.slice 0 4 model.clicked
    in
        case basesymbol of
            "S14c" ->
                handgroupchooser model <| getchoosings basesymbol model.allgroupchoosings

            _ ->
                generalgroupchooser model <| getchoosings basesymbol model.allgroupchoosings


getchoosings : String -> List { c | basesymbol : String, choosings : List a } -> List a
getchoosings basesymbol allgroupchoosings =
    let
        firstfound =
            List.head <| List.filter (\agc -> agc.basesymbol == basesymbol) allgroupchoosings

        choosings =
            case firstfound of
                Just groupchoosings ->
                    groupchoosings.choosings

                Nothing ->
                    []
    in
        choosings



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
