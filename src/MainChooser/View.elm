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


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ div
            [  ]
            (List.map displayChoosing model.choosings)
        , div
            [ style [ "width" => "50%",  "margin-left" => "50%","height" => "250px" , "margin-top" => "5px" ] ]
            [ choosesubgroupchooser model
            ]
        , div
            [ class "generalsymbolchooser", style [ "display" => "inline-block", "margin-top" => "5px" ] ]
            [ generalsymbolchooser model.groupselected [1..6] [1..16] model.selectedcolumn
            ] 
        ]


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


choosesubgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
choosesubgroupchooser model =
    let basesymbol = String.slice 0 4 model.clicked 

    in
    case basesymbol of
        "S14c" ->
            handgroupchooser <| getchoosings basesymbol model.allgroupchoosings
 
        _ ->
            generalgroupchooser <| getchoosings basesymbol model.allgroupchoosings

getchoosings : a -> List { c | basesymbol : a, choosings : List b } -> List b
getchoosings basesymbol allgroupchoosings =
  let firstfound =  List.head <| List.filter (\agc -> agc.basesymbol == basesymbol) allgroupchoosings
      choosings = case firstfound    of
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
