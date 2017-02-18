module SWEditor.DisplaySvg exposing (..)

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Svg exposing (..)
import SW.SymbolConverter exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)


signdisplaysvg : EditorSign -> Html msg
signdisplaysvg sign =
    div []
        (List.map symbolsvg sign.syms)


signdisplaysvgposition : EditorSign -> Int -> Int -> Html msg
signdisplaysvgposition sign offsetx offsety =
    div [ Html.Attributes.style [ "position" => "absolute", "left" => px offsetx, "top" => px offsety ] ]
        (List.map symbolsvg sign.syms)


symbolsvg : EditorSymbol -> Html msg
symbolsvg symbol =
    symbolsvgmargincolor 0 Nothing symbol


symbolsvgmargincolor : Int -> Maybe String -> EditorSymbol -> Html msg
symbolsvgmargincolor margin color symbol =
    let
        symbolheight =
            round <| toFloat symbol.height * symbol.size

        symbolwidth =
            round <| toFloat symbol.width * symbol.size
    in
        svg
            [ attribute "class" "hover background2"
            , attribute "margin" <| px margin
            , attribute "width" <| toString symbolwidth
            , attribute "height" <| toString symbolheight
            , attribute "version" "1.1"
            , attribute "viewBox" <| "0 0 " ++ toString symbolwidth ++ " " ++ toString symbolheight
            , attribute "xmlns" "http://www.w3.org/2000/svg"
            ]
            (symbolview Nothing Nothing color symbol)


symbolview x y color symbol =
    let
        xpos =
            case x of
                Just xvalue ->
                    [ attribute "x" <| toString xvalue ]

                Nothing ->
                    []

        ypos =
            case y of
                Just yvalue ->
                    [ attribute "y" <| toString yvalue ]

                Nothing ->
                    []

        symbolcolor =
            case color of
                Just c ->
                    c

                Nothing ->
                    symbol.nbcolor

        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <|
                fillcodefromkey symbol.key

        defaultfillchararttributes =
            [ attribute "class" "sym-fill"
            , attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';fill:white;font-size:30px;"
            ]

        fillcharattributes =
            List.concat [ xpos, ypos, defaultfillchararttributes ]

        defaultlinechararttributes =
            [ attribute "class" "sym-line"
            , attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';font-size:30px;fill:" ++ symbolcolor ++ ";"
            ]

        linecharattributes =
            List.concat [ xpos, ypos, defaultlinechararttributes ]
    in
        [ Svg.node "text"
            [ attribute "style" "font-size:0%;" ]
            [ Svg.text symbol.key ]
        , g [ attribute "transform" <| scale symbol.size ]
            [ Svg.text_ fillcharattributes
                [ Svg.text fillchar ]
            , Svg.text_ linecharattributes
                [ Svg.text linechar ]
            ]
        ]


scale scalevalue =
    "scale(" ++ toString scalevalue ++ ") "
