// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Posix_Signal from "../Data.Posix.Signal/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
import * as Node_Platform from "../Node.Platform/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
var version = /* #__PURE__ */ (function () {
    return $foreign.process.version;
})();
var stdoutIsTTY = /* #__PURE__ */ (function () {
    return $foreign.process.stdout.isTTY;
})();
var stdout = /* #__PURE__ */ (function () {
    return $foreign.process.stdout;
})();
var stdinIsTTY = /* #__PURE__ */ (function () {
    return $foreign.process.stdin.isTTY;
})();
var stdin = /* #__PURE__ */ (function () {
    return $foreign.process.stdin;
})();
var stderrIsTTY = /* #__PURE__ */ (function () {
    return $foreign.process.stderr.isTTY;
})();
var stderr = /* #__PURE__ */ (function () {
    return $foreign.process.stderr;
})();
var platformStr = /* #__PURE__ */ (function () {
    return $foreign.process.platform;
})();
var platform = /* #__PURE__ */ Node_Platform.fromString(platformStr);
var pid = /* #__PURE__ */ (function () {
    return $foreign.process.pid;
})();
var onSignal = function (sig) {
    return $foreign.onSignalImpl(Data_Posix_Signal.toString(sig));
};
var mkEffect = Unsafe_Coerce.unsafeCoerce;
var nextTick = function (callback) {
    return mkEffect(function (v) {
        return $foreign.process.nextTick(callback);
    });
};
var lookupMutableObject = function (k) {
    return function (o) {
        return mkEffect(function (v) {
            return Foreign_Object.lookup(k)(o);
        });
    };
};
var lookupEnv = function (k) {
    return lookupMutableObject(k)($foreign.process.env);
};
var getEnv = /* #__PURE__ */ (function () {
    return $foreign.copyObject($foreign.process.env);
})();
var execPath = /* #__PURE__ */ mkEffect(function (v) {
    return $foreign.process.execPath;
});
var execArgv = /* #__PURE__ */ (function () {
    return $foreign.copyArray($foreign.process.execArgv);
})();
var cwd = /* #__PURE__ */ (function () {
    return $foreign.process.cwd;
})();
var argv = /* #__PURE__ */ (function () {
    return $foreign.copyArray($foreign.process.argv);
})();
export {
    onBeforeExit,
    onExit,
    onUncaughtException,
    onUnhandledRejection,
    chdir,
    setEnv,
    unsetEnv,
    exit
} from "./foreign.js";
export {
    onSignal,
    nextTick,
    argv,
    execArgv,
    execPath,
    cwd,
    getEnv,
    lookupEnv,
    pid,
    platform,
    stdin,
    stdout,
    stderr,
    stdinIsTTY,
    stdoutIsTTY,
    stderrIsTTY,
    version
};
//# sourceMappingURL=index.js.map
