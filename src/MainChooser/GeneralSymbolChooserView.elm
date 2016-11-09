module MainChooser.GeneralSymbolChooserView exposing (generalsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import SW.Types exposing (..)
 

generalsymbolchooser : ChooserItem -> Fill -> Html Msg
generalsymbolchooser choosing selectedcolumn =
    let
        vf =
            getvalidfills choosing.validfills

        vr =
            getvalidrotations choosing.validrotations
        column = if isValidRotation selectedcolumn vf then
                    selectedcolumn
                else
                    1    
    in 
        div [attribute "ondragstart" "return false;", attribute "ondrop" "return false;"] [  table
                [ Html.Attributes.style
                    [ "width" => "50%"
                    , "height" => px 100
                    , "margin" => "5px"
                    ]
                ]
                [ tr [] (generalsymbolrow choosing.base vf 1)
               
                   ]
                    ,  table
                [ Html.Attributes.style
                    [ "width" => "100%"
                    , "height" => px 100
                    , "margin" => "5px"
                    ]
                ]
                [
                    tr [] (generalsymbolonecolumn choosing.base column 1 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 2 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 3 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 4 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 5 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 6 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 7 vr)
                    , tr [] (generalsymbolonecolumn choosing.base column 8 vr)
                ]
            ] 

getvalidfills : String -> List Fill
getvalidfills validfillsstring =
    case validfillsstring of
        "1 - 6" ->
            [1..6]

        "1 - 4" ->
            [1..4]

        "1, 2" ->
            [1..2]

        "1 - 3" ->
            [1..3]

        "1 - 5" ->
            [1..5]

        "1" ->
            [ 1 ]

        "2" ->
            [ 2 ]

        _ ->
            let a = Debug.log "Could not match valid fills string"  validfillsstring 
            in
                []


getvalidrotations : String -> List Rotation
getvalidrotations validrotationsstring =
    case validrotationsstring of
        "1 - 16" ->
            [1..16]

        "1 - 8" ->
            [1..8]

        "1" ->
            [ 1 ]

        "1 - 4" ->
            [1..4]

        "1, 2, 4, 5, 6, 8" ->
            [ 1, 2, 4, 5, 6, 8 ]

        "1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16" ->
            [ 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 16 ]

        "1 - 6" ->
            [1..6]

        "1, 2" ->
            [1..2]

        "1 - 9" ->
            [1..9]

        _ ->
            let a = Debug.log "Could not match valid rotations string"  validrotationsstring 
            in
            []


generalsymbolonecolumn : Base -> Int -> Int -> List Rotation -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn base symbolcol rotation validrotations =
    let
        rotation1 =
            rotation

        rotation2 =
            rotation + 8

        showrotation1 =
            isValidRotation rotation1 validrotations

        showrotation2 =
            isValidRotation rotation2 validrotations
    in
        [ if showrotation1 then
            td
                [  ]
                [ (generalsymbolcol base symbolcol rotation) ]
          else
            blanktd
        , blanktd
        , if showrotation2 then
            td
                [   Html.Attributes.style

                        [ "text-align" => "center","display" => "block"
                         ,"width" => "45%"
                        ]]
                [ generalsymbolcol base symbolcol rotation2 ]
          else
            blanktd
        ]

 

blanktd : Html a      
blanktd =
    td       []
                []

isValidRotation : a -> List a -> Bool
isValidRotation rotation  validrotations =
    List.any (( ==) rotation) validrotations 


generalsymbolrow : Base -> List Fill -> Rotation -> List (Html MainChooser.Types.Msg)
generalsymbolrow base validfills rotation =
    List.map (\fill -> td [ onClick (SelectedColumn fill) ] [ (generalsymbolcol base fill rotation) ]) validfills


generalsymbolcol : Base -> Fill -> Rotation -> Html MainChooser.Types.Msg
generalsymbolcol base fill rotation =
    let
        symbol =
            getSymbolEditorBaseFillRotation base fill rotation

        sign =
            { syms = [ symbol ]
            }
    in
        -- App.map SymbolView (symbolView "" symbol)
        span [ onMouseDown (DragSymbol symbol.code) ]
            [ App.map SignView
                (signView sign
                    [ Html.Attributes.style
                        [ "position" => "relative"
                        , "left" => px 0
                        , "top" => px 0
                        , "width" => px 20
                        , "height" => px 20
                        , "margin" => "5px"
                        ]
                    ]
                )
            ]
