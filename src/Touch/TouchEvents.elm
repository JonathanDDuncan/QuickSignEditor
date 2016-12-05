effect module Touch.TouchEvents
    where { subscription = MySub }
    exposing
        ( position
        , clicks
        , moves
        , downs
        , ups
        , touchmoves
        , touchdowns
        , touchups
        )

{-| This library lets you listen to global mouse events. This is useful
for a couple tricky scenarios including:

  - Detecting a "click" outside the current component.
  - Supporting drag-and-drop interactions.

# Mouse Position
@docs Position, position

# Subscriptions
@docs clicks, moves, downs, ups

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json exposing ((:=))
import Process
import Task exposing (Task)
import SW.Types exposing (..)


-- POSITIONS


{-| The decoder used to extract a `Position` from a JavaScript mouse event.
-}
position : Json.Decoder Position
position =
    Json.object2 Position ("pageX" := Json.int) ("pageY" := Json.int)



-- MOUSE EVENTS


{-| Subscribe to mouse clicks anywhere on screen.
-}
clicks : (Position -> msg) -> Sub msg
clicks tagger =
    subscription (MySub "click" tagger)


{-| Subscribe to mouse moves anywhere on screen. It is best to unsubscribe if
you do not need these events. Otherwise you will handle a bunch of events for
no benefit.
-}
moves : (Position -> msg) -> Sub msg
moves tagger =
    subscription (MySub "mousemove" tagger)


{-| Get a position whenever the user *presses* the mouse button.
-}
downs : (Position -> msg) -> Sub msg
downs tagger =
    subscription (MySub "mousedown" tagger)


{-| Get a position whenever the user *releases* the mouse button.
-}
ups : (Position -> msg) -> Sub msg
ups tagger =
    subscription (MySub "mouseup" tagger)


touchmoves : (Position -> msg) -> Sub msg
touchmoves tagger =
    subscription (MySub "touchmove" tagger)


{-| Get a position whenever the user *presses* the mouse button.
-}
touchdowns : (Position -> msg) -> Sub msg
touchdowns tagger =
    subscription (MySub "touchstart" tagger)


{-| Get a position whenever the user *releases* the mouse button.
-}
touchups : (Position -> msg) -> Sub msg
touchups tagger =
    subscription (MySub "touchend" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (Position -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (Position -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (Position -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , position : Position
    }


(&>) : Task a b -> Task a c -> Task a c
(&>) t1 t2 =
    t1 `Task.andThen` \_ -> t2


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                `Task.andThen`
                    \state ->
                        Task.succeed
                            (Dict.insert category (Watcher taggers pid) state)

        rightStep category taggers task =
            task
                `Task.andThen`
                    \state ->
                        Process.spawn (Dom.onDocument category position (Platform.sendToSelf router << Msg category))
                            `Task.andThen`
                                \pid ->
                                    Task.succeed
                                        (Dict.insert category (Watcher taggers pid) state)
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, position } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger position)
            in
                Task.sequence (List.map send taggers)
                    `Task.andThen`
                        \_ ->
                            Task.succeed state