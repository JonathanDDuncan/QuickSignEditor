module SWEditor.Icons
    exposing
        ( undoicon
        , redoicon
        , garbagecanicon
        , duplicateicon
        , arrowupicon
        , arrowdownicon
        , arrowrighticon
        , arrowlefticon
        , circleplus
        , circleminus
        )

import Svg exposing (svg, path)
import Html.Attributes exposing (..)
import Html exposing (img)


undoicon : Html.Html msg
undoicon =
    svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "d" "M12.5 8c-2.65 0-5.05.99-6.9 2.6L2 7v9h9l-3.62-3.62c1.39-1.16 3.16-1.88 5.12-1.88 3.54 0 6.55 2.31 7.6 5.5l2.37-.78C21.08 11.03 17.15 8 12.5 8z" ]
            []
        ]


redoicon : Html.Html msg
redoicon =
    svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "d" "M18.4 10.6C16.55 8.99 14.15 8 11.5 8c-4.65 0-8.58 3.03-9.96 7.22L3.9 16c1.05-3.19 4.05-5.5 7.6-5.5 1.95 0 3.73.72 5.12 1.88L13 16h9V7l-3.6 3.6z" ]
            []
        ]


garbagecanicon : Html.Html msg
garbagecanicon =
    svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "d" "M6 19c0 1.1.9 2 2 2h8c1.1 0 2-.9 2-2V7H6v12zM19 4h-3.5l-1-1h-5l-1 1H5v2h14V4z" ]
            []
        ]


circleplus : Html.Html msg
circleplus =
    svg [ attribute "height" "24px", attribute "style" "enable-background:new 0 0 438.533 438.533;", attribute "version" "1.1", attribute "viewBox" "0 0 438.533 438.533", attribute "width" "24px", attribute "x" "0px", attribute "xml:space" "preserve", attribute "xmlns" "http://www.w3.org/2000/svg", attribute "xmlns:xlink" "http://www.w3.org/1999/xlink", attribute "y" "0px" ]
        [ path [ attribute "d" "M409.133,109.203c-19.608-33.592-46.205-60.189-79.798-79.796C295.736,9.801,259.058,0,219.273,0c-39.781,0-76.47,9.801-110.063,29.407c-33.595,19.604-60.192,46.201-79.8,79.796C9.801,142.8,0,179.489,0,219.267c0,39.78,9.804,76.463,29.407,110.062c19.607,33.592,46.204,60.189,79.799,79.798c33.597,19.605,70.283,29.407,110.063,29.407s76.47-9.802,110.065-29.407c33.593-19.602,60.189-46.206,79.795-79.798c19.603-33.596,29.403-70.284,29.403-110.062C438.533,179.485,428.732,142.795,409.133,109.203z M347.179,237.539c0,4.948-1.811,9.236-5.428,12.847c-3.62,3.614-7.901,5.428-12.847,5.428h-73.091v73.084c0,4.948-1.813,9.232-5.428,12.854c-3.613,3.613-7.897,5.421-12.847,5.421h-36.543c-4.948,0-9.231-1.808-12.847-5.421c-3.617-3.621-5.426-7.905-5.426-12.854v-73.084h-73.089c-4.948,0-9.229-1.813-12.847-5.428c-3.616-3.61-5.424-7.898-5.424-12.847v-36.547c0-4.948,1.809-9.231,5.424-12.847c3.617-3.617,7.898-5.426,12.847-5.426h73.092v-73.089c0-4.949,1.809-9.229,5.426-12.847c3.616-3.616,7.898-5.424,12.847-5.424h36.547c4.948,0,9.233,1.809,12.847,5.424c3.614,3.617,5.428,7.898,5.428,12.847v73.089h73.084c4.948,0,9.232,1.809,12.847,5.426c3.617,3.615,5.428,7.898,5.428,12.847V237.539z" ] [] ]


circleminus : Html.Html msg
circleminus =
    svg [ attribute "height" "24px", attribute "style" "enable-background:new 0 0 438.533 438.533;", attribute "version" "1.1", attribute "viewBox" "0 0 438.533 438.533", attribute "width" "24px", attribute "x" "0px", attribute "xml:space" "preserve", attribute "xmlns" "http://www.w3.org/2000/svg", attribute "xmlns:xlink" "http://www.w3.org/1999/xlink", attribute "y" "0px" ]
        [ path [ attribute "d" "M409.133,109.203c-19.608-33.592-46.205-60.189-79.798-79.796C295.736,9.801,259.058,0,219.273,0\x0D\n            c-39.781,0-76.47,9.801-110.063,29.407c-33.595,19.604-60.192,46.201-79.8,79.796C9.801,142.8,0,179.489,0,219.267\x0D\n            c0,39.78,9.804,76.463,29.407,110.062c19.607,33.592,46.204,60.189,79.799,79.798c33.597,19.605,70.283,29.407,110.063,29.407\x0D\n            s76.47-9.802,110.065-29.407c33.593-19.602,60.189-46.206,79.795-79.798c19.603-33.596,29.403-70.284,29.403-110.062\x0D\n            C438.533,179.485,428.732,142.795,409.133,109.203z M347.179,237.539c0,4.948-1.811,9.236-5.428,12.847\x0D\n            c-3.614,3.614-7.898,5.428-12.847,5.428h-219.27c-4.948,0-9.229-1.813-12.847-5.428c-3.616-3.61-5.424-7.898-5.424-12.847v-36.547\x0D\n            c0-4.948,1.809-9.231,5.424-12.847c3.617-3.617,7.898-5.426,12.847-5.426h219.27c4.948,0,9.232,1.809,12.847,5.426\x0D\n            c3.617,3.615,5.428,7.898,5.428,12.847V237.539z" ]
            []
        ]


duplicateicon : Html.Html msg
duplicateicon =
    img [ attribute "width" "24px", attribute "height" "24px", attribute "src" "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAMAAAD04JH5AAAAA3NCSVQICAjb4U/gAAAACXBIWXMAAG66AABuugHW3rEXAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAMlQTFRF////AwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEAwEEZsQYTAAAAEJ0Uk5TAAEEBgcJCgwNExUaHScqNDg/QUlMT1tcYWJkamtxdX+CiImLj5ebnaOksLGys77DztjZ4OTo6ezz9fb4+fr7/P3+UrYKQAAAApVJREFUeNrt2203AkEUB/AR6UEIixKyhBVWi2FWiPn+H8ob56B2ntq59+LMfVud+zundqv/ncvYdFWjOEkzIR1LZGkSR1VWshrdYS5LVD7sNEq0r/WFLF2iX5uzfaXHpZfivco8/Vsj6a1GLff+bS49Fm+79t8dS6813nXsL72Xk6A99g8YO7wLLS4Bilt/EisjCVIj26uxJ4Fq3/L+x6EAD3UrQF+C1ZHV94+AA+RNC0BXAtaeBWAICbgy96/mkICnZSMgkqC1bQTEsIBjIyCBBVwYASksIDUCMljArREAdht6f5u8vjw/GgHOP3ovB4c7m6t15qucut+fbi0xz+VwYz9ZX2D+y7b95GyFgZRl/+s1xggBdxuMUQJumowUcL7IKAGTA9j8wNh/Gzg/ML3kADo/ML3/4PmB4fO/CJ4f6K//Jnx+oH3eBkJ+oL3/YuQHuitwDSM/0DzlDCU/0Nw0VlDyA/XjJ+D5QU8PWAfPD3hNB7hfgM8P+jrAKUJ+IBoawBZGftBRA8QSRn4wVAMuUfKDvKoEDHDyg0gJOMTJD2IlYAcnP0iUgE2s/ED1yCpOfpApAXXo/ODzalcC5s0PXCsAAiAAAiAAAiAA6AGCGpBRA1JqQEINiKkBETVA9ecTDaD6+40H6FIDFBEMHkARQiECimM4REBxEIkJKIxiMQGFYTQqoCiOxwUUDCSQAbMjGWzAzFAKHTA9lsMHTA0mCQA/R7MkgO/DaSLA13ieDlDynFEABEAA/D8A+MCC+Mhn9vsPvYKP7YgPPsfUR78j4sPvucV6HvT4nnYBokO8AiKslhOBD7FQrgFxy9VI2INMf2EVjHwZjn4dkH4hkn4llH4pln4tmJEvRtOvhv+C5XiI4/0f3KirbPVxTQ8AAAAASUVORK5CYII=" ] []


arrowupicon : a -> Html.Html msg
arrowupicon scale =
    svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z" ]
            []
        ]


arrowdownicon : a -> Html.Html msg
arrowdownicon scale =
    svg [ attribute "height" "24", attribute "viewBox" "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M20 12l-1.41-1.41L13 16.17V4h-2v12.17l-5.58-5.59L4 12l8 8 8-8z", attribute "fill" "#010101" ]
            []
        ]


arrowrighticon : a -> Html.Html msg
arrowrighticon scale =
    svg [ attribute "height" "18", attribute "viewBox" "0 0 18 18", attribute "width" "18", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M9 3L7.94 4.06l4.19 4.19H3v1.5h9.13l-4.19 4.19L9 15l6-6z" ]
            []
        ]


arrowlefticon : a -> Html.Html msg
arrowlefticon scale =
    svg [ attribute "height" "18", attribute "viewBox" "0 0 18 18", attribute "width" "18", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ path [ attribute "transform" ("scale(" ++ toString scale ++ ")"), attribute "d" "M15 8.25H5.87l4.19-4.19L9 3 3 9l6 6 1.06-1.06-4.19-4.19H15v-1.5z" ]
            []
        ]
