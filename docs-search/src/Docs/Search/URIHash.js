/* global exports history */

// https://stackoverflow.com/questions/1397329
exports.removeHash = function () {
    var scrollV, scrollH, loc = window.location;
    if ("pushState" in history)
        history.pushState("", document.title, loc.pathname + loc.search);
    else {
        scrollV = document.body.scrollTop;
        scrollH = document.body.scrollLeft;

        loc.hash = "";

        document.body.scrollTop = scrollV;
        document.body.scrollLeft = scrollH;
    }
}
