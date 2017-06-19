module SW.FSW exposing (getFsw)

import SW.Sign exposing (Sign, Lane, getlane, lanes, signinit, centerSign)


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
    in
        boundingbox ++ symbols


symbolsFsw : { c | key : String, x : a, y : b } -> String
symbolsFsw symbol =
    symbol.key ++ toString symbol.x ++ "x" ++ toString symbol.y
