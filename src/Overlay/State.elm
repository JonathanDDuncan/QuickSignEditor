module Overlay.State exposing (init, update, subscriptions, root, Model, Msg)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class)
import Layout.View
import PlatformHelpers exposing (lift)
import Layout.State exposing (Model, Msg(HideOverlay, ShareFsw, PleaseShareFsw))
import Ports exposing (requestSignfromOtherAppDelayed, hideOverlay, showOverlay, shareFsw)
import SW.FSW exposing (getFsw)


init : ( Model, Cmd Msg )
init =
    ( { layout = Tuple.first Layout.State.init
      , show = False
      }
    , Cmd.map Layout (Tuple.second Layout.State.init)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hide ->
            ( { model | show = False }, Cmd.none )

        ShowOverlay str ->
            ( { model | show = True }, requestSignfromOtherAppDelayed "" )

        Layout action ->
            layoutactions action model


layoutactions : Layout.State.Msg -> Model -> ( Model, Cmd Msg )
layoutactions action model =
    case action of
        HideOverlay ->
            ( { model | show = False }, hideOverlay "" )

        ShareFsw ->
            let
                fsw =
                    getFsw model.layout.signbox.sign
            in
                ( { model | show = False }, Cmd.batch [ shareFsw fsw, hideOverlay "" ] )

        PleaseShareFsw _ ->
            let
                fsw =
                    getFsw model.layout.signbox.sign
            in
                ( model, shareFsw fsw )

        _ ->
            lift
                .layout
                (\m x -> { m | layout = x })
                Layout
                Layout.State.update
                action
                model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.State.subscriptions model.layout |> Sub.map Layout
        , showOverlay ShowOverlay
        ]



--View


root : Model -> Html Msg
root model =
    if model.show then
        div [ class "overlay" ]
            [ Html.map Layout (Layout.View.root model.layout)
            ]
    else
        div [] []



--Types


type alias Model =
    { layout : Layout.State.Model
    , show : Bool
    }


type Msg
    = Hide
    | ShowOverlay String
    | Layout Layout.State.Msg
