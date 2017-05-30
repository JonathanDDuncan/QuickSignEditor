// pull in desired CSS/SASS files
require('./assets/styles/main.scss');

// inject bundled Elm app into div#main
//To change aplication name change the three words Starter in the next two line and in the div in index.html
var Elm = require('./QuickSignEditor');
var app = Elm.QuickSignEditor.embed(document.getElementById('quicksigneditor'));
window.app = app;

//subscribe javacript functions to Elm command ports
app.ports.requestSign.subscribe(function (fsw) {
    try {
        var sign = ssw.symbolsList(fsw);
        //send values to Elm subscription ports
        app.ports.receiveSign.send(sign);
    } catch (e) { console.log(e) }

});

app.ports.requestElementPosition.subscribe(function (id) {
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

app.ports.shareFsw.subscribe(function (fsw) {
    try {
        if ("signmaker" in window) {
            signmaker.vm.load(fsw)
        } else {
            window.resultFSW = fsw
            callbackObj.setFsw(fsw);
        }
    } catch (e) { console.log(e) }
});

app.ports.hideOverlay.subscribe(function (val) {
    try {
        if ("signmaker" in window) {
        } else {
            callbackObj.hideOverlay("");
        }
    } catch (e) { console.log(e) }
});

app.ports.requestInitialChoosings.subscribe(function (str) {
    try {
        console.log("requestInitialChoosings called")
        var choosings = getinitialchoosings();
        //send values to Elm subscription ports
        app.ports.receiveInitialChoosings.send(choosings);
    } catch (e) { console.log(e) }
});


app.ports.requestInitialGroupHandChoosings.subscribe(function (str) {
    try {
        console.log("requestInitialGroupHandChoosings called");
        chooserclassification.symbolsizes = symbolsizes;
        app.ports.receiveInitialGroupHandChoosings.send(chooserclassification);

    } catch (e) { console.log(e) }
});
app.ports.cmdDragSymbol.subscribe(function (symbol) {
    app.ports.subDragSymbol.send(symbol);
});
app.ports.cmdAddSymbol.subscribe(function (symbol) {
    app.ports.subAddSymbol.send(symbol);
});
app.ports.cmdReplaceSymbol.subscribe(function (symbol) {
    app.ports.subReplaceSymbol.send(symbol);
});
app.ports.sendKeyboardCommand.subscribe(function (keyboardcommand) {
    app.ports.receiveKeyboardCommand.send(keyboardcommand);
});
app.ports.sendKeyboardMode.subscribe(function (mode) {
    app.ports.receiveKeyboardMode.send(mode);
});


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

        var sign = ssw.symbolsList(fsw);

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
                false, false, false, 0 /*left*/, null);

            first.target.dispatchEvent(simulatedEvent);
        }
    }
}

function init() {
    document.addEventListener("touchstart", touchHandler,  {passive:true});
    document.addEventListener("touchmove", touchHandler,  {passive:true});
    document.addEventListener("touchend", touchHandler,  {passive:true});
    document.addEventListener("touchcancel", touchHandler,  {passive:true});
}

window.onunload = function () {
    app.ports.pleaseShareFsw.send("");
}

init();
