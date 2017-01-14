module SWEditor.Undo exposing (..)

import List.Extra exposing (..)
import SWEditor.Types exposing (..)
import SWEditor.EditorSign exposing (..)


addundoitem : Model -> String -> List UndoItem
addundoitem model actionname =
    (List.append model.undolist [ { actionname = actionname, sign = model.sign } ])


addUndo : Bool -> String -> Model -> Model
addUndo changed actionname model =
    let
        newmodel =
            if changed then
                { model
                    | undolist = (List.append model.undolist [ createundoitem actionname model.sign ])
                    , redolist = []
                }
            else
                model
    in
        newmodel


undo : Model -> Model
undo model =
    let
        undoitem1 =
            (Maybe.withDefault <| createundoitem "" model.sign) <| List.Extra.last model.undolist

        lastsign =
            undoitem1.sign

        len =
            List.length model.undolist

        length =
            if len >= 0 then
                len
            else
                1

        undolist =
            List.take (length - 1) model.undolist

        redolist =
            List.append model.redolist [ createundoitem "Undo" model.sign ]
    in
        { model | sign = lastsign, undolist = undolist, redolist = redolist }


redo : Model -> Model
redo model =
    let
        redoitem1 =
            List.Extra.last model.redolist

        model1 =
            case redoitem1 of
                Just item ->
                    let
                        sign =
                            item.sign

                        len =
                            List.length model.redolist

                        length1 =
                            if len >= 0 then
                                len
                            else
                                1

                        undolist =
                            List.append model.undolist [ createundoitem "Redo" model.sign ]

                        redolist2 =
                            List.take (length1 - 1) model.redolist
                    in
                        { model
                            | sign =
                                sign
                            , undolist =
                                undolist
                            , redolist = redolist2
                        }

                Nothing ->
                    model
    in
        model1


createundoitem : String -> EditorSign -> UndoItem
createundoitem actionname value =
    { actionname = actionname, sign = value }