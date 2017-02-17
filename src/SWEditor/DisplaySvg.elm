module SWEditor.DisplaySvg exposing (..)

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Svg exposing (..)
import SW.SymbolConverter exposing (..)


-- import SWEditor.Types exposing (..)

import SWEditor.EditorSymbol exposing (..)


signdisplaysvg sign offsetx offsety =
    div [ Html.Attributes.style [ "position" => "absolute", "left" => px offsetx, "top" => px offsety ] ]
        (List.map symbolsvg sign.syms)


symbolsvg symbol =
    let
        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <|
                fillcodefromkey symbol.key

        symbolheight =
            round <| toFloat symbol.height * symbol.size

        symbolwidth =
            round <| toFloat symbol.width * symbol.size
    in
        svg [ attribute "class" "hover", attribute "width" <| toString symbolwidth, attribute "height" <| toString symbolheight, attribute "version" "1.1", attribute "viewBox" <| "0 0 " ++ toString symbolwidth ++ " " ++ toString symbolheight, attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ Svg.node "text"
                [ attribute "style" "font-size:0%;" ]
                [ Svg.text symbol.key ]
            , g [ attribute "transform" <| scale symbol.size ]
                [ Svg.text_ [ attribute "class" "sym-fill", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';fill:white;font-size:30px;" ]
                    [ Svg.text fillchar ]
                , Svg.text_
                    [ attribute "class" "sym-line", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';fill:black;font-size:30px;" ]
                    [ Svg.text linechar ]
                ]
            ]


scale scalevalue =
    "scale(" ++ toString scalevalue ++ ") "
