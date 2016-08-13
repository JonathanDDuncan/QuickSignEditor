// pull in desired CSS/SASS files
require('./styles/main.scss');

// inject bundled Elm app into div#main
//To change aplication name change the three words Starter in the next two line and in the div in index.html
var Elm = require('./Starter');
var app = Elm.Starter.embed(document.getElementById('starter'));

//subscribe javacript functions to Elm command ports
// app.ports.check.subscribe(function (word) {
//     var suggestions = spellCheck(word);
//send values to Elm subscription ports
//     app.ports.suggestions.send(suggestions);
// });

