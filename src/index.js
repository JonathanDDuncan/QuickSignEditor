// pull in desired CSS/SASS files
require('./styles/main.scss');

// inject bundled Elm app into div#main
//To change aplication name change the three words Starter in the next two line and in the div in index.html
var Elm = require('./QuickSignEditor');
var app = Elm.QuickSignEditor.embed(document.getElementById('quicksigneditor'));


//subscribe javacript functions to Elm command ports
app.ports.check.subscribe(function(word) {
    var suggestions = sw10.pua(word);
    //send values to Elm subscription ports
    app.ports.suggestions.send(suggestions);
});


app.ports.getpua.subscribe(function(symbol) {
    var pua = sw10.pua(symbol);
    //send values to Elm subscription ports
    app.ports.puaresult.send(pua);
});