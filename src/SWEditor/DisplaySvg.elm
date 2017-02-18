module SWEditor.DisplaySvg exposing (..)

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Svg exposing (..)
import SW.SymbolConverter exposing (..)
import SWEditor.EditorSign exposing (..)
import SWEditor.EditorSymbol exposing (..)


signdisplaysvg : EditorSign -> Int -> Int -> Html msg
signdisplaysvg sign offsetx offsety =
    div [ Html.Attributes.style [ "position" => "absolute", "left" => px offsetx, "top" => px offsety ] ]
        (List.map (symbolsvg 0 Nothing) sign.syms)


symbolsvg :
    Int
    -> Maybe String
    -> EditorSymbol
    -> Html msg
symbolsvg margin color symbol =
    let
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

        symbolheight =
            round <| toFloat symbol.height * symbol.size

        symbolwidth =
            round <| toFloat symbol.width * symbol.size
    in
        svg [ attribute "class" "hover background2", attribute "margin" <| px margin, attribute "width" <| toString symbolwidth, attribute "height" <| toString symbolheight, attribute "version" "1.1", attribute "viewBox" <| "0 0 " ++ toString symbolwidth ++ " " ++ toString symbolheight, attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ Svg.node "text"
                [ attribute "style" "font-size:0%;" ]
                [ Svg.text symbol.key ]
            , g [ attribute "transform" <| scale symbol.size ]
                [ Svg.text_ [ attribute "class" "sym-fill", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';fill:white;font-size:30px;" ]
                    [ Svg.text fillchar ]
                , Svg.text_
                    [ attribute "class" "sym-line", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';font-size:30px;fill:" ++ symbolcolor ++ ";" ]
                    [ Svg.text linechar ]
                ]
            ]


scale scalevalue =
    "scale(" ++ toString scalevalue ++ ") "
