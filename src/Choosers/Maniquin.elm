module Choosers.Maniquin exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onDoubleClick)
import Choosers.Types exposing (Model, Msg(EditorMsg), ChoosingModel)
import Choosers.EditorType as Editor
import Helpers.ViewExtra exposing (px, (=>), calculatescale, transformscale)
import SW.Symbol exposing (Symbol, symbolinit)
import SW.Display


maniquin :
    Model
    -> Int
    -> Int
    -> { divheight : Int, height : Int, view : Html Msg }
maniquin model halfwidth halfheight =
    let
        maniquinwithdimensions =
            choosingsview model.maniquinchoosings 10

        maniquinscale =
            calculatescale (Basics.toFloat maniquinwithdimensions.width)
                (Basics.toFloat maniquinwithdimensions.height)
                (Basics.toFloat halfwidth)
                (Basics.toFloat halfheight)

        maniquindivheight =
            truncate <|
                Basics.toFloat maniquinwithdimensions.height
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
                [ maniquinwithdimensions.display ]
        , height = maniquinwithdimensions.height
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
        [ onClick ((EditorMsg << Editor.Clicked) choosing.value)
        , onMouseDown ((EditorMsg << Editor.DragSymbol) (firstsymbol choosing).key)
        , onDoubleClick
            ((EditorMsg << Editor.ReplaceSymbol) (firstsymbol choosing).key)
        ]
        [ SW.Display.signdisplaysvgposition "hover" choosing.displaySign choosing.offset.offsetx choosing.offset.offsety
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
