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
        var fsw = "M595x704S31430495x510S33e10498x524S30a40513x498S30a50492x498S2ff00480x491S33110507x509S36a10497x541S36d00478x558S37601543x560S37609460x560S14c10551x555S14c18449x552S36d00482x601S20500571x542S21600547x542S22800466x544S2f700558x519S22a00439x646S22e00578x587S23800456x641S24200475x641S24b00503x638S25500534x642S28800437x678S29200460x673S2a200480x673S2a600508x676S2b800554x673S2e300528x672S2ed00563x587S2f100451x528S2f500564x650S33000472x491S36b10461x454S36c00491x447S38500532x443S37e0f443x545--Z01,1.21Z02,1.45Z05,1.51Z07,1.27Z08,1.6Z09,1.6Z10,1.6Z13,1.48Z30,0.79Z33,1.48Z34,0.79Z35,0.79Z36,0.79";
        var choosings = getchoosings(fsw);

        //send values to Elm subscription ports
        app.ports.receiveInitialChoosings.send(choosings);
    } catch (e) { console.log(e) }
});

function getchoosings(fsw) {

    var sign = sw10.symbolsList(fsw);
    var x = 10;
    var y = 20;
    var signs = splitintosigns(sign);
    var choosings = [];

    signs.forEach(function(newsign) {
        choosings.push(getchoosingsign(newsign, x, y));
    });
    return choosings;
}

function splitintosigns(sign) {
    var newsigns = [];
    sign.syms.forEach(function(symbol) {
        newsigns.push(createnewsign(symbol, 500 - sign.x, 500 - sign.y));
    });


    return newsigns;
}

function createnewsign(symbol, x, y) {
    var sign = {};
    sign.syms = [];
    sign.syms.push(symbol);

    sign.backfill =
        "";
    sign.height = symbol.height;
    sign.laned = false;
    sign.left = 0;
    sign.width = symbol.width;

    sign.x = parseInt(symbol.x + x);

    sign.y = parseInt(symbol.y + y);
    sign.text = "";
    return sign;
}

function getchoosingsign(sign, x, y) {


    var offset1 = {};
    offset1.offsetx = sign.x - 500 + x;
    offset1.offsety = sign.y - 500 + y;

    var choosing = {};
    choosing.displaySign = sign;
    choosing.valuestoAdd = sign.syms;
    choosing.value = getchoosingvalue(choosing.valuestoAdd);
    choosing.offset = offset1;
    console.log(JSON.stringify(sign.syms));
    return choosing;
}

function getchoosingvalue(values) {
    var code = 0;
    values.forEach(function(value) {
        code = value.code;
    });
    return code;
}

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
    console.log(JSON.stringify(sign.syms));
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