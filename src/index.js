// pull in desired CSS/SASS files
require('./styles/main.scss');

// inject bundled Elm app into div#main
//To change aplication name change the three words Starter in the next two line and in the div in index.html
var Elm = require('./QuickSignEditor');
var app = Elm.QuickSignEditor.embed(document.getElementById('quicksigneditor'));
window.app = app;

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

        var bodyRect = document.body.getBoundingClientRect(),
            elemRect = element.getBoundingClientRect();

        var result = {};
        result.name = id;
        result.x = Math.trunc(elemRect.left - bodyRect.left);
        result.y = Math.trunc(elemRect.top - bodyRect.top);
        result.width = element.offsetWidth;
        result.height = element.offsetHeight;

        //send values to Elm subscription ports
        app.ports.receiveElementPosition.send(result);
    } catch (e) { console.log(e) }

});

app.ports.shareFsw.subscribe(function(fsw) {
    try {
        if ("signmaker" in window) {
            signmaker.vm.load(fsw)
        } else {
            window.resultFSW = fsw
            callbackObj.setFsw(fsw);
        }
    } catch (e) { console.log(e) }
});


app.ports.requestInitialChoosings.subscribe(function(str) {
    try {
        console.log("requestInitialChoosings called")
        var choosing1 = getchoosing("M518x518S2ff00482x483", 10, 20)
        var choosing2 = getchoosing("M512x516S14c00489x485", 50, 60)

        //send values to Elm subscription ports
        app.ports.receiveInitialChoosings.send([choosing1, choosing2]);
    } catch (e) { console.log(e) }
});

function getchoosing(fsw, offsetx, offsety) {

    var sign = sw10.symbolsList(fsw);
    var offset1 = {};
    offset1.offsetx = offsetx;
    offset1.offsety = offsety;

    var choosing = {};
    choosing.displaySign = sign;
    choosing.valuestoAdd = sign.syms;
    choosing.value = 101;
    choosing.offset = offset1;

    return choosing;
}

app.ports.requestSignfromOtherApp.subscribe(requestSign);

function requestSign(str) {
    try {

        if ("signmaker" in window) {
            var fsw = signmaker.vm.fsw("");
        } else {
            var fsw = window.initialFSW
        }

        var sign = sw10.symbolsList(fsw);

        //send values to Elm subscription ports
        app.ports.receiveSign.send(sign);
    } catch (e) { console.log(e) }
}

function requestSignDelayed(str) {
    try {
        window.setTimeout(requestSign, 15);

    } catch (e) { console.log(e) }
}
app.ports.requestSignfromOtherAppDelayed.subscribe(requestSignDelayed);


function touchHandler(event) {
    var touches = event.changedTouches,
        first = touches[0],
        type = "";
    switch (event.type) {
        case "touchstart":
            type = "mousedown";
            break;
        case "touchmove":
            type = "mousemove";
            break;
        case "touchend":
            type = "mouseup";
            break;
        default:
            return;
    }
    var simulatedEvent = document.createEvent("MouseEvent");
    simulatedEvent.initMouseEvent(type, true, true, window, 1,
        first.screenX, first.screenY,
        first.clientX, first.clientY, false,
        false, false, false, 0 /*left*/ , null);

    first.target.dispatchEvent(simulatedEvent);
    event.preventDefault();
}

function init() {
    document.addEventListener("touchstart", touchHandler, true);
    document.addEventListener("touchmove", touchHandler, true);
    document.addEventListener("touchend", touchHandler, true);
    document.addEventListener("touchcancel", touchHandler, true);
}

window.onunload = function() {
    app.ports.pleaseShareFsw.send("");
}

init();