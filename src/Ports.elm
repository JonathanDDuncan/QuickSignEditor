port module Ports
    exposing
        ( requestSign
        , requestInitialGroupHandChoosings
        , requestInitialChoosings
        , sendKeyboardMode
        , cmdaddsigntosignview
        , cmdAddSymbol
        , cmdDragSymbol
        , cmdReplaceSymbol
        , receiveInitialChoosings
        , receiveInitialGroupHandChoosings
        , receiveKeyboardCommand
        , receiveSign
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
        , shareFsw
        )

import SW.Types exposing (..)
import Choosers.Types as Choosers exposing (..)
import Keyboard.Shared exposing (KeyboardCommand)


-- Ports go here like this


port requestSign : String -> Cmd msg


port receiveSign : (PortableSign -> msg) -> Sub msg


port requestSignfromOtherApp : String -> Cmd msg


port receiveSignfromOtherApp : (PortableSign -> msg) -> Sub msg


port receiveInitialChoosings : (List Choosers.ChoosingImportModel -> msg) -> Sub msg


port receiveInitialGroupHandChoosings : (Choosers.HandGroupImportModel -> msg) -> Sub msg


port requestElementPosition : String -> Cmd msg


port receiveElementPosition : (NamedPosition -> msg) -> Sub msg


port shareFsw : String -> Cmd msg


port hideOverlay : String -> Cmd msg


port pleaseShareFsw : (String -> msg) -> Sub msg


port requestSignfromOtherAppDelayed : String -> Cmd msg


port requestInitialChoosings : String -> Cmd msg


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
