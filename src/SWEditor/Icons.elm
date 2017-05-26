module SWEditor.Icons exposing (..)

import Svg exposing (svg, path)
import Html.Attributes exposing (..)
import Html exposing (img, div)
import ViewHelper.ViewExtra exposing (..)


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