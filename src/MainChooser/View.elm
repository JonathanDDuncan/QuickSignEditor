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


--import SubMainChooser.View exposing (root)


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ div []
            (List.map displayChoosing model.choosings)
        , div
            [ style [ "width" => "50%", "height" => "150px", "margin-left" => "50%", "margin-top" => "5px" ] ]
            [ choosesubgroupchooser model
            ]
        , div
            [ class "generalsymbolchooser", style [ "width" => "50%", "height" => "150px", "margin-left" => "50%", "margin-top" => "5px" ] ]
            [ generalsymbolchooser model
            ]
        ]


choosesubgroupchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
choosesubgroupchooser model =
    case model.clicked of
        "S14c10" ->
            handgroupchooser model

        "S14c18" ->
            handgroupchooser model

        _ ->
            nogroupchooser model


generalsymbolchooser : MainChooser.Types.Model -> Html MainChooser.Types.Msg
generalsymbolchooser model =
    let
        group =
            1

        -- model.selectedgroup
        validfills =
            [1..6]

        validrotations =
            [1..16]
    in
        table []
            [ generalsymbolrow group validfills 1
            , generalsymbolrow group validfills 2
            , generalsymbolrow group validfills 3
            , generalsymbolrow group validfills 4
            , generalsymbolrow group validfills 5
            , generalsymbolrow group validfills 6
            , generalsymbolrow group validfills 7
            , generalsymbolrow group validfills 8
            , generalsymbolrow group validfills 9
            , generalsymbolrow group validfills 10
            , generalsymbolrow group validfills 11
            , generalsymbolrow group validfills 12
            , generalsymbolrow group validfills 13
            , generalsymbolrow group validfills 14
            , generalsymbolrow group validfills 15
            , generalsymbolrow group validfills 16
            ]


generalsymbolrow group validfills rotation =
    tr
        [ style
            [ "height" => "50px" ]
        ]
        [ generalsymbolcol group rotation 1
        , generalsymbolcol group rotation 2
        , generalsymbolcol group rotation 3
        , generalsymbolcol group rotation 4
        , generalsymbolcol group rotation 5
        , generalsymbolcol group rotation 6
        ]


generalsymbolcol group rotation fill =
    let
        symbol =
            getSymbolEditor group fill rotation
    in
        App.map SymbolView (symbolView "" symbol)


getSymbolEditor group fill rotation =
    let
        key =
            "S" ++ toString group ++ toString fill ++ toString rotation

        symbol =
            { x = 0
            , y = 0
            , width = 20
            , height = 20
            , fontsize = 30
            , size = 1
            , nwcolor = ""
            , pua = ""
            , code = 0
            , key = key
            , nbcolor = ""
            }
    in
        toEditorSymbol 0 0 symbol



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
