module Choosers.State exposing (init, update, subscriptions)

import Choosers.Types exposing (Msg(..), handsymbolinit)
import Choosers.ChooserItemType exposing (chooseriteminit)
import Choosers.KeyboardType as KeyboardType
import Choosers.LoadingType as Loading
import SW.Symbol exposing (createSymbolbyBaseFillRotation)
import Ports exposing (requestInitialGroupHandChoosings, subLoadManiquinChoosings, loadGroupChoosings, receiveKeyboardCommand, loadPortableSign)
import Dict
import Material
import Choosers.HandSymbolChooser exposing (createflowersymbols, gethandfillitems)
import Update.Extra
import Choosers.Loading exposing (loadingupdate)
import Choosers.EditorUpdate exposing (editorupdate)
import Choosers.Keyboard exposing (keyboardupdate)
import SW.HandsType as Hands exposing (Hands(..))
import SW.HandFillsType as HandFills exposing (HandFills(..))


init : ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
init =
    ( { lastmdlid = 0
      , mdl = Material.model
      , maniquinchoosings = []
      , clicked = ""
      , selectedcolumn = 1
      , groupchoosings =
            [ { basesymbol = ""
              , choosings = []
              }
            ]
      , groupselected = chooseriteminit
      , handgroupfilter = 1
      , symbolsizes = Dict.empty
      , handsymbol = handsymbolinit
      , chooserskeyboard =
            { maniquinkeyboard = []
            , groupchooserkeyboard = []
            , symbolchooserkeyboard = []
            , keyboardpage = 1
            }
      }
    , Cmd.batch [ requestInitialGroupHandChoosings "" ]
    )


update : Choosers.Types.Msg -> Choosers.Types.Model -> ( Choosers.Types.Model, Cmd Choosers.Types.Msg )
update action model =
    case action of
        Noop ->
            ( model
            , Cmd.none
            )

        Mdl msg ->
            Material.update Mdl msg model

        EditorMsg msg ->
            editorupdate msg model update

        KeyboardMsg msg ->
            keyboardupdate msg model update

        LoadingMsg msg ->
            loadingupdate msg model

        FilterHandGroup value ->
            ( { model | handgroupfilter = value }
            , Cmd.none
            )

        SelectHand hand ->
            let
                handfill =
                    case hand of
                        Hands.Left ->
                            case model.handsymbol.handfill of
                                HandFills.RightBack ->
                                    HandFills.LeftBack

                                HandFills.RightThumbEdge ->
                                    HandFills.LeftThumbEdge

                                HandFills.RightPalm ->
                                    HandFills.LeftPalm

                                HandFills.RightBabyEdge ->
                                    HandFills.LeftBabyEdge

                                _ ->
                                    model.handsymbol.handfill

                        Hands.Right ->
                            case model.handsymbol.handfill of
                                HandFills.LeftBack ->
                                    HandFills.RightBack

                                HandFills.LeftThumbEdge ->
                                    HandFills.RightThumbEdge

                                HandFills.LeftPalm ->
                                    HandFills.RightPalm

                                HandFills.LeftBabyEdge ->
                                    HandFills.RightBabyEdge

                                _ ->
                                    model.handsymbol.handfill

                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol | hand = hand, handfill = handfill }
            in
                ( { model | handsymbol = newhandsymbol }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateHandSymbolChooser

        SelectPlane plane ->
            let
                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol | plane = plane }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateHandSymbolChooser

        SelectHandFill handfill ->
            let
                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol | handfill = handfill }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update UpdateHandSymbolChooser

        UpdateHandSymbolChooser ->
            let
                handsymbol =
                    model.handsymbol

                newhandsymbol =
                    { handsymbol
                        | flowersymbols = createflowersymbols model.handsymbol model.groupselected.base model.symbolsizes
                        , symbollefthand = createSymbolbyBaseFillRotation model.groupselected.base 3 9 model.symbolsizes
                        , symbolrighthand = createSymbolbyBaseFillRotation model.groupselected.base 3 1 model.symbolsizes
                        , handfillitems = gethandfillitems model.groupselected.base model.symbolsizes model.handsymbol.hand model.handsymbol.plane
                    }
            in
                ( { model
                    | handsymbol = newhandsymbol
                  }
                , Cmd.none
                )
                    |> Update.Extra.andThen update ((KeyboardMsg <| KeyboardType.UpdateChooserKeyboards))


subscriptions : Sub Choosers.Types.Msg
subscriptions =
    Sub.batch
        [ subLoadManiquinChoosings (LoadingMsg << Loading.LoadManiquinChoosings)
        , loadGroupChoosings (LoadingMsg << Loading.LoadGroupChoosings)
        , receiveKeyboardCommand (KeyboardMsg << KeyboardType.Keyboard)
        , loadPortableSign (LoadingMsg << Loading.LoadPortableSign)
        ]
