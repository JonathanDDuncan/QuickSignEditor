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
            [ style [  "height" => "250px"   ] ]
            (List.map displayChoosing model.choosings) 
             , div
            [ class "generalsymbolchooser", style [ "display" => "inline-block", "margin-top" => "5px", "float" => "left"] ]
            [ generalsymbolchooser model.groupselected model.selectedcolumn model.symbolsizes
            ] ,
       div
            [ style ["position" => "absolute", "width" => "300px", "left" => "-50px","top" => "0px", "margin-left" => "50%","height" => "550px" , "margin-top" => "5px", "overflow-y" => "scroll", "overflow-x" => "scroll" ] ]
            [ choosesubgroupchooser model model.symbolsizes
            ] 
      
        ]


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]

 
choosesubgroupchooser model symbolsizes =
    let basesymbol = String.slice 0 4 model.clicked 

    in
    case basesymbol of
        "S14c" ->
            handgroupchooser model.handgroupfilter symbolsizes <| getchoosings basesymbol model.allgroupchoosings
 
        _ -> 
            generalgroupchooser  symbolsizes <| getchoosings basesymbol model.allgroupchoosings

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
