port module Ports exposing (..)

import SW.Types exposing (..)
import Choosing.Types as Choosing exposing (..)
import MainChooser.Types as MainChooser exposing (..)


-- Ports go here like this


port requestSign : String -> Cmd msg


port receiveSign : (Sign -> msg) -> Sub msg


port requestSignfromOtherApp : String -> Cmd msg


port receiveSignfromOtherApp : (Sign -> msg) -> Sub msg


port receiveInitialChoosings : (List Choosing.ImportModel -> msg) -> Sub msg


port receiveInitialGroupHandChoosings : (MainChooser.HandGroupImportModel -> msg) -> Sub msg


port requestElementPosition : String -> Cmd msg


port receiveElementPosition : (NamedPosition -> msg) -> Sub msg


port shareFsw : String -> Cmd msg


port pleaseShareFsw : (String -> msg) -> Sub msg


port requestSignfromOtherAppDelayed : String -> Cmd msg


port requestInitialChoosings : String -> Cmd msg


port requestInitialGroupHandChoosings : String -> Cmd msg
