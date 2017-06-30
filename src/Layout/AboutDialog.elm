module Layout.AboutDialog exposing (aboutdialog)

import Html exposing (..)
import Html.Attributes exposing (..)
import Layout.State exposing (Model, Msg(Mdl, ShareFsw, HideOverlay), iswidescreen, ismediumscreen)
import Material.Dialog as Dialog
import Material.Button as Button
import Material.Options as Options
import Markdown
import Helpers.ViewExtra exposing (..)


aboutdialog : Model -> Html Msg
aboutdialog model =
    Dialog.view
        [ Options.css "width" "800px"
        , Options.css "width" "500px"
        ]
        [ Dialog.title [] [ text "About ..." ]
        , Dialog.content [ Options.css "overflow-y" "scroll" ]
            [ p [ style [ "height" => px 300, "margin-top" => px 10 ] ] [ Markdown.toHtml [] markdown ]
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.colored
                , Dialog.closeOn "click"
                ]
                [ text "Ok" ]
            ]
        ]


markdown =
    """**QuickSignEditor** is [Licensed under the MIT License](https://github.com/JonathanDDuncan/QuickSignEditor/blob/master/LICENSE)\x0D
by Jonathan Duncan\x0D
The latest version of this project is hosted at https://jonathandduncan.github.io/QuickSignEditor/\x0D
\x0D
You may have to install the Sutton SignWriting Fonts below on your computer to use it.\x0D
\x0D
There is a version that can run inside SignMaker2015 \x0D
(http://signbank.org/signmaker.html https://github.com/Slevinski/signmaker)\x0D
is available at https://jonathandduncan.github.io/QuickSignEditor/signmaker/#?ui=en\x0D
\x0D
A version also runs inside the SignWriter Studio, Windows program which can be downloaded at http://www.signwriterstudio.com/\x0D
\x0D
For more information about SignWriting see http://www.SignWriting.org\x0D
\x0D
\x0D
### Acknowledgements\x0D
 ![Valerie Sutton](./assets/img/Valerie_Sutton.jpg)\x0D
\x0D
 The kind support of [Valerie Sutton](http://www.signwriting.org/encyclopedia/team.html#ValerieSutton) the Inventor of SignWriting for her ongoing support throughout the project and for inventing SignWriting.\x0D
 \x0D
http://www.valeriesutton.org/\x0D
\x0D
http://www.SignWriting.org\x0D
\x0D
----------\x0D
\x0D
\x0D
![Steve Slevinski](./assets/img/Steve_Slevinski.jpg)\x0D
\x0D
[Steve Slevinski](http://www.signwriting.org/encyclopedia/team.html#SteveSlevinski)\x0D
Sutton SignWriting Fonts\x0D
\x0D
Copyright (c) 1974-2016, Center for Sutton Movement Writing, inc\x0D
\x0D
Licensed under the SIL Open Font License v1.1\x0D
\x0D
https://slevinski.github.io/SuttonSignWriting/components/fonts.html\x0D
\x0D
Sutton SignWriting JavaScript Library\x0D
\x0D
Copyright (c) 2007-2016, Stephen E Slevinski Jr\x0D
\x0D
Licensed under the MIT License\x0D
\x0D
https://slevinski.github.io/SuttonSignWriting/components/js.html\x0D
\x0D
----------\x0D
\x0D
![Adam Frost](./assets/img/Adam_Frost.jpg)\x0D
\x0D
Hand images photographed by [Adam Frost](http://www.signwriting.org/encyclopedia/team.html#AdamFrost).\x0D
http://www.movementwriting.org/symbolbank/downloads/ISWA2010/ISWA2010_Photos\x0D
"""
