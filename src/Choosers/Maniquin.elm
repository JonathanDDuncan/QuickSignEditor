module Choosers.Maniquin exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick, onMouseDown, onDoubleClick)
import Choosers.Types exposing (Model, Msg(Editor, SignView), ChoosingModel)
import Choosers.Types as Editor exposing (Editor)
import Helpers.ViewExtra exposing (px, (=>), calculatescale, transformscale)
import SW.Symbol exposing (Symbol, symbolinit, iskey)
import SW.Display


maniquin :
    Model
    -> Int
    -> Int
    -> { divheight : Int, height : Int, view : Html Msg }
maniquin model halfwidth halfheight =
    let
        maniquin =
            choosingsview model.maniquinchoosings 10

        maniquinscale =
            calculatescale (Basics.toFloat maniquin.width)
                (Basics.toFloat maniquin.height)
                (Basics.toFloat halfwidth)
                (Basics.toFloat halfheight)

        maniquindivheight =
            truncate <|
                Basics.toFloat maniquin.height
                    * maniquinscale
    in
        { view =
            div
                [ style
                    [ ( "transform-origin", "top left" )
                    , transformscale maniquinscale
                    , "height" => px maniquindivheight
                    , "width" => px halfwidth
                      -- , "margin-top" => px -10
                    ]
                ]
                [ maniquin.display ]
        , height = maniquin.height
        , divheight = maniquindivheight
        }


choosingsview : List ChoosingModel -> Int -> { display : Html Choosers.Types.Msg, width : Int, height : Int }
choosingsview choosings bottompadding =
    let
        size =
            getchoosingssize choosings
    in
        { display =
            div
                [ style [ "position" => "relative" ]
                ]
                (List.map displayChoosing choosings)
        , width = size.right - size.left
        , height = size.bottom - size.top + bottompadding
        }


displayChoosing : ChoosingModel -> Html Choosers.Types.Msg
displayChoosing choosing =
    div
        [ onClick ((Editor << Editor.Clicked) choosing.value)
        , onMouseDown ((Editor << Editor.DragSymbol) (firstsymbol choosing).key)
        , onDoubleClick
            ((Editor << Editor.ReplaceSymbol) (firstsymbol choosing).key)
        ]
        [ Html.map SignView
            (SW.Display.signdisplaysvgposition "hover" choosing.displaySign choosing.offset.offsetx choosing.offset.offsety)
        ]


getchoosingssize :
    List ChoosingModel
    -> { bottom : Int, left : Int, right : Int, top : Int }
getchoosingssize choosings =
    List.foldr
        (\choosing accumulator ->
            let
                newright =
                    Basics.max accumulator.right (choosing.offset.offsetx + choosing.displaySign.width)

                newleft =
                    Basics.min accumulator.left choosing.offset.offsetx

                newbottom =
                    Basics.max accumulator.bottom (choosing.offset.offsety + choosing.displaySign.height)

                newtop =
                    Basics.min accumulator.top choosing.offset.offsety
            in
                { top = newtop, bottom = newbottom, right = newright, left = newleft }
        )
        { top = 0, bottom = 0, right = 0, left = 0 }
        choosings


firstsymbol : { b | valuestoAdd : List Symbol } -> Symbol
firstsymbol choosing =
    choosing.valuestoAdd |> List.head |> Maybe.withDefault symbolinit
