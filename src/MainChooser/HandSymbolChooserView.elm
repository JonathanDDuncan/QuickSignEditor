module MainChooser.HandSymbolChooserView exposing (handsymbolchooser)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App exposing (..)
import ViewHelper.ViewExtra exposing (..)
import SWEditor.EditorSymbol exposing (..)
import MainChooser.Types exposing (..)
import SWEditor.Display exposing (signView)
import SW.Types exposing (..)
import Dict exposing (..)

handsymbolchooser : HandSymbol -> ChooserItem ->  Dict String Size -> Int ->Int -> Html Msg
handsymbolchooser handsymbol choosing  symbolsizes width height  =
    let 
        rowheight = truncate <| toFloat height / toFloat 10
    in 
        div [attribute "ondragstart" "return false;", attribute "ondrop" "return false;"]
         [
              table 
                [class "symbolchooserheader", Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px  rowheight
                    ]
                    ]
               [tr [] <| List.append ( handselection handsymbol choosing.base   symbolsizes  rowheight )
                ( planeselection handsymbol choosing.base   symbolsizes  rowheight )]
             ,table 
                [class "symbolchooserheader", Html.Attributes.style
                    [ "width" => px (width - 12)
                    , "height" => px  rowheight
                    ]
                ]
                [ tr [] (fillsview handsymbol choosing.base symbolsizes rowheight  )
               
                   ] 
                   
            ]
fillsview     : { a | hand : Hands }    -> Base     -> Dict String Size     -> Int     -> List (Html Msg)
fillsview handsymbol base symbolsizes rowheight =
    let handfills = if handsymbol.hand == Right then
                [{fill =2, rotation = 9},{fill =1, rotation = 1},{fill =2, rotation = 1},{fill =3, rotation = 1}]
                else
                  [{fill =2, rotation = 1},{fill =1, rotation = 1},{fill =2, rotation = 9},{fill =3, rotation = 1}]
    in 
      List.map (\handfill -> td [ onClick (SelectHandFill handfill) ] [ (generalsymbolcol False base handfill.fill handfill.rotation symbolsizes  rowheight ) ]) handfills

handselection :HandSymbol ->  Base -> Dict String Size ->Int ->  List (Html MainChooser.Types.Msg)
handselection handsymbol base symbolsizes  rowheight  =
   [td [ onClick (SelectHand Left), selectedbackground Left handsymbol.hand] [ (generalsymbolcol False base 3 9 symbolsizes  rowheight ) ,  div [] [text "Left"]] ,
   td [ onClick (SelectHand Right) , selectedbackground Right handsymbol.hand] [ (generalsymbolcol False base 3 1 symbolsizes  rowheight )  , div [] [ text "Right"]] ] 

planeselection :HandSymbol -> Base -> Dict String Size ->Int ->   List (Html MainChooser.Types.Msg)
planeselection handsymbol base symbolsizes  rowheight  =
   [td [ onClick (SelectPlane Wall), selectedbackground Wall handsymbol.plane ] [ img [src "./img/wallplane.png", width 70]  [] , div [] [text "Wall"]] ,
   td [ onClick (SelectPlane Floor), selectedbackground Floor handsymbol.plane ] [ img [src "./img/floorplane.png", width 70]  [] , div [] [text "Floor"]] ] 

selectedbackground : a -> a -> Attribute b
selectedbackground expected currentselected =
    if expected == currentselected then
        style[ "background" => "#7b85c0"]
    else
        style  [] 
    

getscales columnwidth rowheight symbols =
    List.minimum (List.map (\symbol -> calcscale symbol.width symbol.height  columnwidth rowheight) symbols)

getsymbols base fills rotations symbolsizes =
    let 
        symbols = List.concatMap (\rotation -> List.map (\fill -> getSymbolEditorBaseFillRotation base fill rotation symbolsizes) fills) rotations
    in
        symbols

generalsymbolonecolumn : Base -> Int -> Int -> List Rotation -> Dict String Size ->  Int -> Int -> Float -> List (Html MainChooser.Types.Msg)
generalsymbolonecolumn base symbolcol rotation validrotations symbolsizes columnwidth rowheight scale =
    let
        rotation1 =
            rotation

        rotation2 =
            rotation + 8

        showrotation1 =
            isValidRotation rotation1 validrotations

        showrotation2 =
            isValidRotation rotation2 validrotations
    in
        [ if showrotation1 then
            td 
                [  ]
                [ (generalsymbolcol True base symbolcol rotation symbolsizes    rowheight  )  ]
          else
            blanktd
        , blanktd
        , if showrotation2 then
            td
                [   Html.Attributes.style
 
                        [ "text-align" => "center","display" => "block"
                         ,"width" => "45%"
                        ]]
                [ generalsymbolcol True base symbolcol rotation2  symbolsizes   rowheight  ]
          else
            blanktd
        ]

 

blanktd : Html a      
blanktd =
    td       []
                []

isValidRotation : a -> List a -> Bool
isValidRotation rotation  validrotations =
    List.any (( ==) rotation) validrotations 

 

generalsymbolcol : Bool -> Base -> Fill -> Rotation -> Dict String Size -> Int ->   Html MainChooser.Types.Msg
generalsymbolcol drag base fill rotation symbolsizes    rowheight   =
    let
        symbol =
            getSymbolEditorBaseFillRotation base fill rotation symbolsizes

        sign =
            { syms = [ symbol ]
            }
    in 
        -- App.map SymbolView (symbolView "" symbol)
        div [
            onMouseDown ( if (drag) then DragSymbol symbol.code else Noop)   
          ]
        [
       
             App.map SignView
                (signView sign
                    [ Html.Attributes.style
                        
                        [ "position" => "relative"
                        , "margin"   => "auto" 
                        , "left" => px 0
                        , "top" => px 4
                        , "width" => px symbol.width
                        , "height" => px symbol.height                                               ] 
                    ]
                )
 ]

scaling scale =
    if scale <= 1 then
     [  "transform" => ("scale(" ++ toString scale ++ ")")]
    else
     []

calcscale  swidth sheight columnwidth rowheight =            
    Basics.min (toFloat columnwidth / toFloat swidth) (toFloat rowheight / toFloat swidth)