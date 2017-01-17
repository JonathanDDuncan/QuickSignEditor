// pull in desired CSS/SASS files
require('./assets/styles/main.scss');

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

app.ports.hideOverlay.subscribe(function(val) {
    try {
        if ("signmaker" in window) {
        } else {
            callbackObj.hideOverlay("");
        }
    } catch (e) { console.log(e) }
});

app.ports.requestInitialChoosings.subscribe(function(str) {
    try {
        console.log("requestInitialChoosings called")
        var fsw = "M600x697S33000476x489S2ff00484x489S30a50495x495S30a40516x495S31430498x506S33e10501x524S33110510x511S36d10481x558S36a10500x541S36d00481x558S37601546x560S37609463x560S14c10554x555S20500576x542S21600552x542S22800463x558S2f700562x519S22a00470x636S22e00583x587S23800487x631S24200506x631S24b00534x628S28800468x668S29900491x663S2e300534x665S2ed00568x587S2f100448x542S2f500564x655S36b10463x454S36c00493x447S38500534x443S37e0f440x559S29600511x663S38700476x695--Z01,1.48Z02,1.51Z05,1.21Z06,1.45Z08,1.63Z09,1.27Z10,1.6Z11,1.6Z12,1.6Z26,0.79Z29,0.79Z30,0.79Z31,0.79";

        var sign = sw10.symbolsList(fsw);
        var x = 10;
        var y = 10;
        var signs = splitintosigns(sign);


        var newsigns = [];
        sign.syms.forEach(function(symbol) {
            newsigns.push(createnewsign(symbol, 500 - sign.x, 500 - sign.y));
        });

        var choosings = [];
        newsigns.forEach(function(newsign) {
            choosings.push(getchoosingsign(newsign, x, y));
        });
        //send values to Elm subscription ports
        app.ports.receiveInitialChoosings.send(choosings);
    } catch (e) { console.log(e) }
});

app.ports.requestInitialGroupHandChoosings.subscribe(function(str) {
    try {
        console.log("requestInitialGroupHandChoosings called");
        chooserclassification.symbolsizes = symbolsizes;
        app.ports.receiveInitialGroupHandChoosings.send(chooserclassification);

    } catch (e) { console.log(e) }
});
app.ports.cmdDragSymbol.subscribe(function(symbol) {
    app.ports.subDragSymbol.send(symbol);
});
app.ports.cmdReplaceSymbol.subscribe(function(symbol) {
    app.ports.subReplaceSymbol.send(symbol);
});
app.ports.sendKeyboardCommand.subscribe(function(keyboardcommand) {
    app.ports.receiveKeyboardCommand.send(keyboardcommand);
});


function getchoosings(fsw) {

    var sign = sw10.symbolsList(fsw);
    var x = 10;
    var y = 10;
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
    // console.log(JSON.stringify(sign.syms));
    return choosing;
}

function getchoosingvalue(values) {
    var key = 0;
    values.forEach(function(value) {
        key = value.key;
    });
    return key;
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
    // console.log(JSON.stringify(sign.syms));
    return choosing;
}

function getgrouphandchoosings() {

    var fistbabycommon = "M510x509S19210489x490";
    var fistringcommon = "M510x512S18d10490x487";
    var fistmiddlecommon = "M515x652S11010497x413S11510495x347S11a10495x518S11910498x450S11e10492x586S12d10491x550S14010485x622S10e10494x479S11810499x382";
    var fistindexcommon = "M515x590S10010489x410S10610494x469S10a10493x442S1dc10491x503S1eb10485x571S1ea10490x544";
    var fistthumbcommon = "M511x549S20310490x451S14a10491x534S1f510492x510S1f710491x489S1f810491x469";
    var circlethumbcommon = "M508x508S17610492x492";
    var circleindexcommon = "M508x515S10110492x485";
    var circleringcommon = "M509x515S18710491x486";
    var circlebabycommon = "M513x548S1a510492x520S1bb10488x490S1ce10490x452";
    var cupbabycommon = "M508x533S15310492x508S15410493x467";
    var cupthumbcommon = "M509x510S16d10492x490";
    var cupindexcommon = "M510x510S16c10490x489";
    var anglethumbcommon = "M516x563S18510485x518S18210484x438S18010486x458S18110495x534S17d10485x478S17e10486x500";
    var anglebabycommon = "M516x532S1c510484x504S1d410487x467";
    var flatthumbcommon = "M510x545S15a10490x456S14710492x523S15d10491x487";
    var flatbabycommon = "M513x551S14410489x449S15010488x520S14e10487x485";


    grouphandchoosings = {};
    grouphandchoosings.fistbabycommon = getchoosings(fistbabycommon);
    grouphandchoosings.fistringcommon = getchoosings(fistringcommon);
    grouphandchoosings.fistmiddlecommon = getchoosings(fistmiddlecommon);
    grouphandchoosings.fistindexcommon = getchoosings(fistindexcommon);
    grouphandchoosings.fistthumbcommon = getchoosings(fistthumbcommon);
    grouphandchoosings.circlethumbcommon = getchoosings(circlethumbcommon);
    grouphandchoosings.circleindexcommon = getchoosings(circleindexcommon);
    grouphandchoosings.circleringcommon = getchoosings(circleringcommon);
    grouphandchoosings.circlebabycommon = getchoosings(circlebabycommon);
    grouphandchoosings.cupbabycommon = getchoosings(cupbabycommon);
    grouphandchoosings.cupthumbcommon = getchoosings(cupthumbcommon);
    grouphandchoosings.cupindexcommon = getchoosings(cupindexcommon);
    grouphandchoosings.anglethumbcommon = getchoosings(anglethumbcommon);
    grouphandchoosings.anglebabycommon = getchoosings(anglebabycommon);
    grouphandchoosings.flatthumbcommon = getchoosings(flatthumbcommon);
    grouphandchoosings.flatbabycommon = getchoosings(flatbabycommon);

    return grouphandchoosings;
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
    var touches = event.changedTouches
    if (touches) {
        var first = touches[0],
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
        if (first) {
            var simulatedEvent = document.createEvent("MouseEvent");
            simulatedEvent.initMouseEvent(type, true, true, window, 1,
                first.screenX, first.screenY,
                first.clientX, first.clientY, false,
                false, false, false, 0 /*left*/ , null);

            first.target.dispatchEvent(simulatedEvent);

            // event.preventDefault();
        }
    }
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