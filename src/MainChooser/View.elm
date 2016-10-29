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
import SWEditor.EditorSign exposing (..)
import SW.SymbolConverter exposing (..)
import ParseInt as ParseInt exposing (..)


--import SubMainChooser.View exposing (root)


displayChoosing : Choosing.Types.Model -> Html MainChooser.Types.Msg
displayChoosing choosing =
    div [ onClick (Clicked choosing.value) ] [ App.map Choosing (Choosing.View.root choosing) ]


root : MainChooser.Types.Model -> Html MainChooser.Types.Msg
root model =
    div []
        [ div
            [ style [ "display" => "inline-block", "margin-top" => "5px" ] ]
            (List.map displayChoosing model.choosings)
        , div
            [ style [ "width" => "50%", "height" => "150px", "margin-left" => "50%", "margin-top" => "5px" ] ]
            [ choosesubgroupchooser model
            ]
        , div
            [ class "generalsymbolchooser", style [ "display" => "inline-block", "margin-top" => "5px" ] ]
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
        table
            [ Html.Attributes.style
                [ "width" => "50%"
                , "height" => px 100
                , "margin" => "5px"
                ]
            ]
            [ tr [] (generalsymbolrow group validfills 1)
            , tr [] (generalsymbolrow group validfills 2)
            , tr [] (generalsymbolrow group validfills 3)
            , tr [] (generalsymbolrow group validfills 4)
            , tr [] (generalsymbolrow group validfills 5)
            , tr [] (generalsymbolrow group validfills 6)
            , tr [] (generalsymbolrow group validfills 7)
            , tr [] (generalsymbolrow group validfills 8)
            , tr [] (generalsymbolrow group validfills 9)
            , tr [] (generalsymbolrow group validfills 10)
            , tr [] (generalsymbolrow group validfills 11)
            , tr [] (generalsymbolrow group validfills 12)
            , tr [] (generalsymbolrow group validfills 13)
            , tr [] (generalsymbolrow group validfills 14)
            , tr [] (generalsymbolrow group validfills 15)
            , tr [] (generalsymbolrow group validfills 16)
            ]


generalsymbolrow : Int -> a -> Int -> List (Html MainChooser.Types.Msg)
generalsymbolrow group validfills rotation =
    [ td [] [ generalsymbolcol group rotation 1 ]
    , td [] [ generalsymbolcol group rotation 2 ]
    , td [] [ generalsymbolcol group rotation 3 ]
    , td [] [ generalsymbolcol group rotation 4 ]
    , td [] [ generalsymbolcol group rotation 5 ]
    , td [] [ generalsymbolcol group rotation 6 ]
    ]


generalsymbolcol : Int -> Int -> Int -> Html MainChooser.Types.Msg
generalsymbolcol group rotation fill =
    let
        symbol =
            getSymbolEditor group fill rotation

        sign =
            { syms = [ symbol ]
            }
    in
        -- App.map SymbolView (symbolView "" symbol)
        App.map SignView
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


getSymbolEditor : Int -> Int -> Int -> EditorSymbol
getSymbolEditor group fill rotation =
    let
        base =
            group + 255

        key =
            Debug.log "key" ("S" ++ (ParseInt.toRadix' 16 base) ++ (ParseInt.toRadix' 16 (fill - 1)) ++ (ParseInt.toRadix' 16 (rotation - 1)))

        pua =
            Debug.log "pua" (SW.SymbolConverter.pua key)

        code =
            Debug.log "code" (SW.SymbolConverter.codefromkey key)

        symbol =
            { x = 0
            , y = 0
            , width = 20
            , height = 20
            , fontsize = 30
            , size = 1
            , nwcolor = "white"
            , pua = pua
            , code = code
            , key = key
            , nbcolor = "black"
            }
    in
        toEditorSymbol 0 0 symbol



-- "margin-left" => (toString (20 * (col - 1)) ++ "%"),


nogroupchooser : a -> Html b
nogroupchooser model =
    div []
        [ text "nogroupchooser" ]
