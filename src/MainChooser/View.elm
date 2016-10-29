module MainChooser.View exposing (root)

import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MainChooser.Types exposing (..)
import Choosing.View exposing (..)
import Choosing.Types exposing (..)
import ViewHelper.ViewExtra exposing (..)
import MainChooser.HandGroupChooserView exposing (..)
import SWEditor.Display exposing (..)
import SWEditor.EditorSymbol exposing (..)
import SW.SymbolConverter exposing (..)
import ParseInt as ParseInt exposing (..)
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
            [ generalsymbolchooser model
            ]
        ]


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


choosesubgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
choosesubgroupchooser model =
    case model.clicked of
        "S14c10" ->
            handgroupchooser model

        "S14c18" ->
            handgroupchooser model

        _ ->
            nogroupchooser model



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
