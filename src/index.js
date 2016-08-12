// pull in desired CSS/SASS files
require('./styles/main.scss');

// inject bundled Elm app into div#main
var Elm = require('./Starter');
var app = Elm.Starter.embed(document.getElementById('starter'));

var spellCheck = function (word) {
    return ["The word '" + word + "' has been spell  checked, well not really ;-)"];
}

app.ports.check.subscribe(function (word) {
    var suggestions = spellCheck(word);
    app.ports.suggestions.send(suggestions);
});

