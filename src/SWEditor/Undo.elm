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
                    | undolist = Debug.log "Undo list" <| (List.append model.undolist [ createundoitem actionname model.sign ])
                    , redolist = Debug.log "Redo list" <| []
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
            Debug.log "Undo list" <|
                List.take (length - 1) model.undolist

        redolist =
            Debug.log "Redo list" <|
                List.append model.redolist [ createundoitem "Undo" model.sign ]

        actionname =
            Debug.log "Undo actionname" undoitem1.actionname
    in
        { model | sign = lastsign, undolist = undolist, redolist = redolist }


redo : Model -> Model
redo model =
    let
        redoitem1 =
            List.Extra.last model.redolist

        model =
            case redoitem1 of
                Just item ->
                    let
                        sign =
                            item.sign

                        len =
                            List.length model.redolist

                        length =
                            if len >= 0 then
                                len
                            else
                                1

                        undolist =
                            Debug.log "Undo list" <|
                                List.append model.undolist [ createundoitem "Redo" model.sign ]

                        redolist =
                            Debug.log "Redo list" <|
                                List.take (length - 1) model.redolist

                        actionname =
                            Debug.log "Undo actionname" item.actionname
                    in
                        { model | sign = sign, undolist = undolist, redolist = redolist }

                Nothing ->
                    model
    in
        model


createundoitem : String -> EditorSign -> UndoItem
createundoitem actionname value =
    { actionname = actionname, sign = value }
