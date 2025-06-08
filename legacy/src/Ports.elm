port module Ports
    exposing
        ( requestSign
        , requestInitialGroupHandChoosings
        , cmdRequestChoosings
        , sendKeyboardMode
        , cmdaddsigntosignview
        , cmdAddSymbol
        , cmdDragSymbol
        , cmdReplaceSymbol
        , subLoadManiquinChoosings
        , loadGroupChoosings
        , receiveKeyboardCommand
        , loadPortableSign
        , requestSignfromOtherApp
        , requestElementPosition
        , subaddsigntosignview
        , receiveElementPosition
        , subDragSymbol
        , subAddSymbol
        , subReplaceSymbol
        , sendKeyboardCommand
        , receiveKeyboardMode
        , pleaseShareFsw
        , requestSignfromOtherAppDelayed
        , hideOverlay
        , showOverlay
        , shareFsw
        , cmdchangenormallywhite
        , subchangenormallywhite
        , cmdchangenormallyblack
        , subchangenormallyblack
        )

import SW.Types exposing (NamedPosition)
import SW.PortableSign exposing (PortableSign)
import SW.Symbol exposing (Symbol)
import Choosers.ImportModelType as Choosers
import Keyboard.Shared exposing (KeyboardCommand)


-- Ports go here like this


port requestSign : String -> Cmd msg


port loadPortableSign : (PortableSign -> msg) -> Sub msg


port requestSignfromOtherApp : String -> Cmd msg


port subLoadManiquinChoosings : (List Choosers.ChoosingImportModel -> msg) -> Sub msg


port loadGroupChoosings : (Choosers.HandGroupImportModel -> msg) -> Sub msg


port requestElementPosition : String -> Cmd msg


port receiveElementPosition : (NamedPosition -> msg) -> Sub msg


port shareFsw : String -> Cmd msg


port hideOverlay : String -> Cmd msg


port showOverlay : (String -> msg) -> Sub msg


port pleaseShareFsw : (String -> msg) -> Sub msg


port requestSignfromOtherAppDelayed : String -> Cmd msg


port cmdRequestChoosings : String -> Cmd msg


port requestInitialGroupHandChoosings : String -> Cmd msg


port cmdDragSymbol : Symbol -> Cmd msg


port subDragSymbol : (Symbol -> msg) -> Sub msg


port cmdAddSymbol : Symbol -> Cmd msg


port subAddSymbol : (Symbol -> msg) -> Sub msg


port cmdReplaceSymbol : Symbol -> Cmd msg


port subReplaceSymbol : (Symbol -> msg) -> Sub msg


port sendKeyboardCommand : KeyboardCommand -> Cmd msg


port receiveKeyboardCommand : (KeyboardCommand -> msg) -> Sub msg


port sendKeyboardMode : Int -> Cmd msg


port receiveKeyboardMode : (Int -> msg) -> Sub msg


port cmdaddsigntosignview : PortableSign -> Cmd msg


port subaddsigntosignview : (PortableSign -> msg) -> Sub msg


port cmdchangenormallywhite : String -> Cmd msg


port subchangenormallywhite : (String -> msg) -> Sub msg


port cmdchangenormallyblack : String -> Cmd msg


port subchangenormallyblack : (String -> msg) -> Sub msg
