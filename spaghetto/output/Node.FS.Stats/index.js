// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_JSDate from "../Data.JSDate/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var Stats = /* #__PURE__ */ (function () {
    function Stats(value0) {
        this.value0 = value0;
    };
    Stats.create = function (value0) {
        return new Stats(value0);
    };
    return Stats;
})();
var statusChangedTime = function (v) {
    return fromJust(Data_JSDate.toDateTime(v.value0.ctime));
};
var showStats = {
    show: function (v) {
        return "Stats " + $foreign.showStatsObj(v.value0);
    }
};
var modifiedTime = function (v) {
    return fromJust(Data_JSDate.toDateTime(v.value0.mtime));
};
var isSymbolicLink = function (v) {
    return $foreign.statsMethod("isSymbolicLink", v.value0);
};
var isSocket = function (v) {
    return $foreign.statsMethod("isSocket", v.value0);
};
var isFile = function (v) {
    return $foreign.statsMethod("isFile", v.value0);
};
var isFIFO = function (v) {
    return $foreign.statsMethod("isFIFO", v.value0);
};
var isDirectory = function (v) {
    return $foreign.statsMethod("isDirectory", v.value0);
};
var isCharacterDevice = function (v) {
    return $foreign.statsMethod("isCharacterDevice", v.value0);
};
var isBlockDevice = function (v) {
    return $foreign.statsMethod("isBlockDevice", v.value0);
};
var accessedTime = function (v) {
    return fromJust(Data_JSDate.toDateTime(v.value0.atime));
};
export {
    Stats,
    isFile,
    isDirectory,
    isBlockDevice,
    isCharacterDevice,
    isFIFO,
    isSocket,
    isSymbolicLink,
    accessedTime,
    modifiedTime,
    statusChangedTime,
    showStats
};
//# sourceMappingURL=index.js.map
