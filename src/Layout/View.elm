module Layout.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Layout.Types exposing (..)


--import SubLayout.View exposing (root)

import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)
import Material.Footer as Footer
import Material.Options as Options
import Material.Layout as Layout


root1 : Model -> Html Msg
root1 model =
    Layout.render Mdl
        model.mdl
        []
        { header = []
        , drawer = []
        , tabs = ( [], [] )
        , main = []
        }


root : Model -> Html Msg
root model =
    div [ class "fullheight" ]
        [ mysignBox model
        , stickyFooter model
        ]


mysignBox : Model -> Html Msg
mysignBox model =
    div [ style [ ( "min-width", "300px" ), ( "min-heigth", "300px" ), ( "width", "300px" ), ( "heigth", "300px" ), ( "background-color", "blue" ) ] ] [ text "Hey" ]


stickyFooter : Model -> Html Msg
stickyFooter model =
    div
        [ class "footer"
          -- style [ ( "clear", "both" ), ( "position", "relative" ), ( "z-index", "1000" ), ( "height", "3em" ), ( "margin-top", "-3em" ) ]
        ]
        [ text "this is the sticky footer"
        ]


viewBody : Model -> Html Msg
viewBody model =
    Material.Scheme.top <|
        div
            [ style [ ( "padding", "2rem" ) ] ]
            [ text ("Current count: " ++ toString model.count)
              {- We construct the instances of the Button component that we need, one
                 for the increase button, one for the reset button. First, the increase
                 button. The first three arguments are:
                   - A Msg constructor (`Mdl`), lifting Mdl messages to the Msg type.
                   - An instance id (the `[0]`). Every component that uses the same model
                     collection (model.mdl in this file) must have a distinct instance id.
                   - A reference to the elm-mdl model collection (`model.mdl`).
                 Notice that we do not have to add fields for the increase and reset buttons
                 separately to our model; and we did not have to add to our update messages
                 to handle their internal events.
                 Mdl components are configured with `Options`, similar to `Html.Attributes`.
                 The `Button.onClick Increase` option instructs the button to send the `Increase`
                 message when clicked. The `css ...` option adds CSS styling to the button.
                 See `Material.Options` for details on options.
              -}
            , Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.onClick Increase
                , css "margin" "0 24px"
                ]
                [ text "Increase" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.onClick Reset ]
                [ text "Reset" ]
            ]



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.
