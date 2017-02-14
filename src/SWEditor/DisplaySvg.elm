module SWEditor.DisplaySvg exposing (..)

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Svg exposing (..)
import SW.SymbolConverter exposing (..)


-- import SWEditor.Types exposing (..)

import SWEditor.EditorSymbol exposing (..)


signdisplaysvg sign offsetx offsety =
    div [ class "", Html.Attributes.style [ "position" => "absolute", "left" => px offsetx, "top" => px offsety ] ]
        (List.map symbolsvg sign.syms)


symbolsvg symbol =
    let
        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <| fillcodefromkey symbol.key
    in
        svg [ attribute "width" <| toString symbol.width, attribute "height" <| toString symbol.height, attribute "version" "1.1", attribute "viewBox" <| "500 500 " ++ toString symbol.width ++ " " ++ toString symbol.height, attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ Svg.node "text"
                [ attribute "style" "font-size:0%;" ]
                [ Svg.text symbol.key ]
            , g [ attribute "transform" "translate(500,500)" ]
                [ Svg.text_ [ attribute "class" "sym-fill", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';fill:white;font-size:" ++ toString symbol.fontsize ++ "px;" ]
                    [ Svg.text fillchar ]
                , Svg.text_
                    [ attribute "class" "sym-line", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';fill:black;font-size:" ++ toString symbol.fontsize ++ "px;" ]
                    [ Svg.text linechar ]
                ]
            ]
