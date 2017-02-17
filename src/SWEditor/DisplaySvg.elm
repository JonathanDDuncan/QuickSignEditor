module SWEditor.DisplaySvg exposing (..)

import Html as Html exposing (..)
import Html.Attributes exposing (..)
import ViewHelper.ViewExtra exposing (..)
import Svg exposing (..)
import SW.SymbolConverter exposing (..)


-- import SWEditor.Types exposing (..)

import SWEditor.EditorSymbol exposing (..)


signdisplaysvg sign offsetx offsety =
    div []
        (List.map (symbolsvg offsetx offsety) sign.syms)


symbolsvg offsetx offsety symbol =
    let
        linechar =
            puaCharCode <| linecodefromkey symbol.key

        fillchar =
            puaCharCode <| fillcodefromkey symbol.key

        centerX =
            toFloat symbol.width / 2

        centerY =
            toFloat symbol.height / 2

        heightdiff =
            truncate <| ((toFloat symbol.height) * (symbol.size - 2.0) - toFloat symbol.height)

        scale =
            -- translate (0.0 - centerX * symbol.size) (0.0 - centerY * symbol.size)
            --     ++
            scales symbol.size

        --  ++ translate (0.0 - centerX) (0.0 - centerY)
        -- ++ translate (0.0 - centerX) (0.0 - centerY)
        -- ++ translate (centerX * symbol.size) (centerY * symbol.size)
        -- ++ translate (centerX) (centerY)
        symbolheight =
            round <| toFloat symbol.height * symbol.size

        symbolwidth =
            round <| toFloat symbol.width * symbol.size
    in
        div [ Html.Attributes.style [ "position" => "absolute", "left" => px offsetx, "top" => px offsety ] ]
            [ svg [ attribute "class" "hover", attribute "width" <| toString symbolwidth, attribute "height" <| toString symbolheight, attribute "version" "1.1", attribute "viewBox" <| "0 0 " ++ toString symbolwidth ++ " " ++ toString symbolheight, attribute "xmlns" "http://www.w3.org/2000/svg" ]
                [ Svg.node "text"
                    [ attribute "style" "font-size:0%;" ]
                    [ Svg.text symbol.key ]
                , g [ attribute "transform" scale ]
                    [ -- Svg.rect
                      -- [ attribute "width" "20", attribute "height" "20", attribute "fill" "blue" ]
                      -- []
                      Svg.text_ [ attribute "class" "sym-fill", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';fill:white;font-size:30px;" ]
                        [ Svg.text fillchar ]
                    , Svg.text_
                        [ attribute "class" "sym-line", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';fill:black;font-size:30px;" ]
                        [ Svg.text linechar ]
                    ]
                  -- , g []
                  --     [ -- Svg.rect
                  --       -- [ attribute "width" "20", attribute "height" "20", attribute "fill" "blue" ]
                  --       -- []
                  --       Svg.text_ [ attribute "class" "sym-fill", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';fill:white;font-size:30px;" ]
                  --         [ Svg.text fillchar ]
                  --     , Svg.text_
                  --         [ attribute "class" "sym-line", attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';fill:black;font-size:30px;" ]
                  --         [ Svg.text linechar ]
                  --     ]
                ]
            ]


translate x y =
    "translate(" ++ toString x ++ "," ++ toString y ++ ") "


scales scalevalue =
    "scale(" ++ toString scalevalue ++ ") "
