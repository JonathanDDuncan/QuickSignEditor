module SW.Display
    exposing
        ( symbolsvg
        , symbolsvgmargincolor
        , symbolsvgscale
        , signsvgnoscale
        , signdisplaysvg
        , signdisplaysvgposition
        )

import Html exposing (Html, div)
import Html.Attributes exposing (attribute)
import Helpers.ViewExtra exposing (px, (=>))
import Svg exposing (Svg, svg, g)
import SW.Pua exposing (puaCharCode, linecodefromkey, fillcodefromkey)
import SW.Sign exposing (Sign)
import SW.Symbol exposing (Symbol)
import ParseInt exposing (parseIntHex)


signdisplaysvg : String -> Sign -> Html msg
signdisplaysvg symbolclass sign =
    div []
        (List.map (symbolsvg symbolclass) sign.syms)


signdisplaysvgposition : String -> Sign -> Int -> Int -> Html msg
signdisplaysvgposition symbolclass sign offsetx offsety =
    div [ Html.Attributes.style [ "position" => "absolute", "left" => px offsetx, "top" => px offsety ] ]
        (List.map (symbolsvg symbolclass) sign.syms)


symbolsvg : String -> Symbol -> Html msg
symbolsvg class symbol =
    symbolsvgmargincolor 0 Nothing 1 class symbol


symbolsvgscale : Float -> String -> Symbol -> Html msg
symbolsvgscale scale class symbol =
    symbolsvgmargincolor 0 Nothing scale class symbol


symbolsvgmargincolor : Int -> Maybe String -> Float -> String -> Symbol -> Html msg
symbolsvgmargincolor margin color scale class symbol =
    let
        symbolheight =
            round <| toFloat symbol.height * symbol.size * scale

        symbolwidth =
            round <| toFloat symbol.width * symbol.size * scale
    in
        svg
            [ attribute "class" class
            , attribute "margin" <| px margin
            , attribute "width" <| toString symbolwidth
            , attribute "height" <| toString symbolheight
            , attribute "version" "1.1"
            , attribute "viewBox" <| "0 0 " ++ toString symbolwidth ++ " " ++ toString symbolheight
            , attribute "xmlns" "http://www.w3.org/2000/svg"
            ]
            (symbolview Nothing Nothing color scale symbol)


signsvgnoscale : Sign -> Html msg
signsvgnoscale sign =
    signsvg 1 sign


signsvg : Float -> Sign -> Html msg
signsvg scale sign =
    let
        signheight =
            round <| toFloat sign.height * scale

        signwidth =
            round <| toFloat sign.width * scale
    in
        svg
            [ attribute "width" <| toString signwidth
            , attribute "height" <|
                toString signheight
            , attribute
                "version"
                "1.1"
            , attribute "viewBox" <| "0 0 " ++ toString signwidth ++ " " ++ toString signheight
            , attribute "xmlns" "http://www.w3.org/2000/svg"
            ]
            (List.concat
                (List.map (\symbol -> symbolview (Just symbol.x) (Just symbol.y) Nothing 1 symbol) sign.syms)
            )


symbolview : Maybe a -> Maybe b -> Maybe String -> Float -> Symbol -> List (Svg msg)
symbolview x y color scale symbol =
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
            Maybe.withDefault symbol.nbcolor color

        linechar =
            symbol.key
                |> linecodefromkey
                |> puaCharCode

        fillchar =
            symbol.key
                |> fillcodefromkey
                |> puaCharCode

        defaultfillchararttributes =
            [ attribute "class" "sym-fill"
            , attribute "style" <| "pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:" ++ fixcolor symbol.nwcolor ++ ";"
            ]

        fillcharattributes =
            List.concat [ xpos, ypos, defaultfillchararttributes ]

        defaultlinechararttributes =
            [ attribute "class" "sym-line"
            , attribute "style" <| "pointer-events:none;font-family:'SuttonSignWriting';font-size:30px;fill:" ++ fixcolor symbolcolor ++ ";"
            ]

        linecharattributes =
            List.concat [ xpos, ypos, defaultlinechararttributes ]
    in
        [ Svg.node "text"
            [ attribute "style" "font-size:0%;" ]
            [ Svg.text symbol.key ]
        , g [ attribute "transform" <| scaling (symbol.size * scale) ]
            [ Svg.text_ fillcharattributes
                [ Svg.text fillchar ]
            , Svg.text_ linecharattributes
                [ Svg.text linechar ]
            ]
        ]


scaling : a -> String
scaling scalevalue =
    "scale(" ++ toString scalevalue ++ ") "


fixcolor : String -> String
fixcolor color =
    if ishexvalue color then
        "#" ++ color
    else
        color


ishexvalue : String -> Bool
ishexvalue value =
    case parseIntHex value of
        Ok _ ->
            True

        Err _ ->
            False
