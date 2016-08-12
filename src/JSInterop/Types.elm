module JSInterop.Types exposing (..)

type alias Model =
  { word : String
  , suggestions : List String
  }


type Msg
  = Change String
  | Check
  | Suggest (List String)
