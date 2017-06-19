module Choosers.LoadingType exposing (Loading(..))

import SW.PortableSign exposing (PortableSign)
import Choosers.ImportModelType exposing (HandGroupImportModel, ChoosingImportModel)


type Loading
    = LoadManiquinChoosings (List ChoosingImportModel)
    | LoadGroupChoosings HandGroupImportModel
    | LoadPortableSign PortableSign
