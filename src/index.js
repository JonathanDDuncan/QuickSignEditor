// pull in desired CSS/SASS files
require('./styles/main.scss');

// inject bundled Elm app into div#main
//To change aplication name change the three words Starter in the next two line and in the div in index.html
var Elm = require('./QuickSignEditor');
var app = Elm.QuickSignEditor.embed(document.getElementById('quicksigneditor'));


//subscribe javacript functions to Elm command ports
app.ports.requestSign.subscribe(function(fsw) {
    try {
        var sign = sw10.symbolsList(fsw);
        //send values to Elm subscription ports
        app.ports.receiveSign.send(sign);
    } catch (e) { console.log(e) }

});
app.ports.requestElementPosition.subscribe(function(id) {
    try {
        var element = document.getElementById(id);
        var x = element.offsetLeft;
        var y = element.offsetTop;

        var result = {};
        result.name = id;
        result.x = x;
        result.y = y;
        result.width = y;
        result.height = y;

        //send values to Elm subscription ports
        app.ports.receiveElementPosition.send(result);
    } catch (e) { console.log(e) }

});