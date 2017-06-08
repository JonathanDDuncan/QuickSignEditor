module PlatformHelpers exposing (lift)

import Task


-- excerpted from https://github.com/debois/elm-mdl/blob/66a1f2c10cb3850686eb610174710e9fcf176425/src/Material/Helpers.elm


{-| Variant of EA update function type, where effects may be
lifted to a different type.
-}
type alias Update_ model action action_ =
    action -> model -> ( model, Cmd action_ )


{-| Standard EA update function type.
-}
type alias Update model action =
    Update_ model action action


{-| Variant of `lift` for effect-free components.
-}
lift_ :
    (model -> submodel)
    -> -- get
       (model -> submodel -> model)
    -> -- set
       (subaction -> submodel -> submodel)
    -> subaction
    -> -- action
       model
    -> -- model
       ( model, Cmd action )
lift_ get set update action model =
    ( set model (update action (get model)), Cmd.none )


{-| Convenience function for writing update-function boilerplate. Example use:
    case msg of
      ...
      ButtonsMsg msg' ->
        lift .buttons (\m x->{m|buttons=x}) ButtonsMsg Demo.Buttons.update msg' model
This is equivalent to the more verbose
    case msg of
      ...
      ButtonsMsg msg' ->
        let
          (buttons', cmd) =
            Demo.Buttons.update msg' model.buttons
        in
          ( { model | buttons = buttons'}
          , Cmd.map ButtonsMsg cmd
          )
-}
lift :
    (model -> submodel)
    -> -- get
       (model -> submodel -> model)
    -> -- set
       (subaction -> action)
    -> -- fwd
       Update submodel subaction
    -> -- update
       subaction
    -> -- action
       model
    -> -- model
       ( model, Cmd action )
lift get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, Cmd.map fwd e )


{-|
  Lift any value of type `msg` to a `Cmd msg`.
-}
cmd : msg -> Cmd msg
cmd msg =
    Task.perform (always msg) (Task.succeed msg)
