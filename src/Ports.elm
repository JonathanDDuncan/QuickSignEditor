port module Ports exposing (..)

import SW.Types exposing (..)
import Choosers.Types as Choosers exposing (..)
import Keyboard.Shared exposing (KeyboardCommand)


-- Ports go here like this


port requestSign : String -> Cmd msg


port receiveSign : (Sign -> msg) -> Sub msg


port requestSignfromOtherApp : String -> Cmd msg


port receiveSignfromOtherApp : (Sign -> msg) -> Sub msg


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
