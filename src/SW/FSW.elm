module SW.FSW exposing (getFsw)

import SW.Sign exposing (Sign, Lane, getlane, lanes, signinit, centerSign)
import SW.Symbol exposing (Symbol)


getFsw : Sign -> String
getFsw sign =
    let
        centered =
            centerSign 500 500 sign

        boundingbox =
            sign.spelling
                ++ "M"
                ++ toString (500 + (round <| toFloat centered.width / 2))
                ++ "x"
                ++ toString (500 + (round <| toFloat centered.height / 2))

        symbols =
            List.foldr (++) "" (List.map symbolsFsw centered.syms)

        styling =
            symbolstylingFsw centered.syms

        fsw =
            Debug.log "getFsw" <| boundingbox ++ symbols ++ styling
    in
        fsw


symbolsFsw : Symbol -> String
symbolsFsw symbol =
    symbol.key ++ toString symbol.x ++ "x" ++ toString symbol.y


symbolstylingFsw : List Symbol -> String
symbolstylingFsw symbols =
    let
        zoom =
            zoomstyling symbols

        colors =
            colorstyling symbols
    in
        if zoom ++ colors == "" then
            ""
        else
            "--" ++ zoom ++ colors


colorstyling : List Symbol -> String
colorstyling symbols =
    List.foldr (++) "" (List.indexedMap colorstyle symbols)


colorstyle : Int -> Symbol -> String
colorstyle index symbol =
    let
        black =
            if symbol.nbcolor /= "black" then
                symbol.nbcolor
            else
                ""

        white =
            if symbol.nwcolor /= "white" then
                symbol.nwcolor
            else
                ""

        symboltocolor =
            "C" ++ twodigits (index + 1)
    in
        if black ++ white == "" then
            ""
        else if white == "" then
            symboltocolor ++ "_" ++ black ++ "_"
        else if black == "" then
            symboltocolor ++ "_" ++ "black," ++ white ++ "_"
        else
            symboltocolor ++ "_" ++ black ++ "," ++ white ++ "_"


zoomstyling : List { a | size : number } -> String
zoomstyling symbols =
    List.foldr (++) "" (List.indexedMap zoomstyle symbols)


zoomstyle : Int -> { a | size : number } -> String
zoomstyle index symbol =
    if symbol.size /= 1 then
        "Z" ++ twodigits (index + 1) ++ "," ++ toString symbol.size
    else
        ""


twodigits : Int -> String
twodigits value =
    let
        str =
            toString value

        len =
            String.length str

        toadd =
            2 - len
    in
        List.range 1 toadd
            |> List.map (\_ -> "0")
            |> List.foldr (++) ""
            |> (\chain -> chain ++ str)
