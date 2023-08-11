// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HTTP_Method from "../Data.HTTP.Method/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_MediaType from "../Data.MediaType/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Uncurried from "../Effect.Uncurried/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
import * as Web_XHR_ReadyState from "../Web.XHR.ReadyState/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var toEnum = /* #__PURE__ */ Data_Enum.toEnum(Web_XHR_ReadyState.boundedEnumReadyState);
var un = /* #__PURE__ */ Data_Newtype.un();
var xmlHttpRequest = /* #__PURE__ */ Effect_Uncurried.runEffectFn1($foreign["_xmlHttpRequest"]);
var withCredentials = function (xhr) {
    return function () {
        return $foreign["_getProperty"]("withCredentials", xhr);
    };
};
var upload = function (xhr) {
    return function () {
        return $foreign["_getProperty"]("upload", xhr);
    };
};
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var timeout = function (xhr) {
    return function () {
        return $foreign["_getProperty"]("timeout", xhr);
    };
};
var statusText = function (xhr) {
    return function () {
        return $foreign["_getProperty"]("statusText", xhr);
    };
};
var status = function (xhr) {
    return function () {
        return $foreign["_getProperty"]("status", xhr);
    };
};
var setWithCredentials = function (wc) {
    return function (xhr) {
        return function () {
            return $foreign["_setProperty"]("withCredentials", wc, xhr);
        };
    };
};
var setTimeout = function (ms) {
    return function (xhr) {
        return function () {
            return $foreign["_setProperty"]("timeout", ms, xhr);
        };
    };
};
var setRequestHeader = function (header) {
    return function (value) {
        return function (xhr) {
            return function () {
                return $foreign["_setRequestHeader"](header, value, xhr);
            };
        };
    };
};
var sendString = function (payload) {
    return function (xhr) {
        return function () {
            return $foreign["_send"](payload, xhr);
        };
    };
};
var sendFormData = function (payload) {
    return function (xhr) {
        return function () {
            return $foreign["_send"](payload, xhr);
        };
    };
};
var sendDocument = function (payload) {
    return function (xhr) {
        return function () {
            return $foreign["_send"](payload, xhr);
        };
    };
};
var sendBlob = function (payload) {
    return function (xhr) {
        return function () {
            return $foreign["_send"](payload, xhr);
        };
    };
};
var sendArrayView = function (payload) {
    return function (xhr) {
        return function () {
            return $foreign["_send"](payload, xhr);
        };
    };
};
var send = function (xhr) {
    return function () {
        return $foreign["_send"](Data_Nullable.toNullable(Data_Maybe.Nothing.value), xhr);
    };
};
var responseURL = function (xhr) {
    return function () {
        return $foreign["_getProperty"]("responseURL", xhr);
    };
};
var response = function (xhr) {
    return map(Data_Nullable.toMaybe)(function () {
        return $foreign["_getProperty"]("response", xhr);
    });
};
var readyState = function (xhr) {
    var toReadyState = function (rs) {
        return Data_Maybe.fromMaybe(Web_XHR_ReadyState.Unsent.value)(toEnum(rs));
    };
    return map(toReadyState)(function () {
        return $foreign["_getProperty"]("readyState", xhr);
    });
};
var overrideMimeType = function (ty) {
    return function (req) {
        return function () {
            return $foreign["_overrideMimeType"](un(Data_MediaType.MediaType)(ty), req);
        };
    };
};
var open$prime = function (options) {
    return function (xhr) {
        return function () {
            return $foreign["_open"](Data_HTTP_Method.print(options.method), options.url, Data_Nullable.toNullable(options.username), Data_Nullable.toNullable(options.password), xhr);
        };
    };
};
var open = function (method) {
    return function (url) {
        return function (xhr) {
            return function () {
                return $foreign["_open"](Data_HTTP_Method.print(method), url, Data_Nullable.toNullable(Data_Maybe.Nothing.value), Data_Nullable.toNullable(Data_Maybe.Nothing.value), xhr);
            };
        };
    };
};
var getResponseHeader = function (header) {
    return function (xhr) {
        return map(Data_Nullable.toMaybe)(function () {
            return $foreign["_getResponseHeader"](header, xhr);
        });
    };
};
var getAllResponseHeaders = function (xhr) {
    return map(Data_Nullable.toMaybe)(function () {
        return $foreign["_getAllResponseHeaders"](xhr);
    });
};
var abort = /* #__PURE__ */ Effect_Uncurried.runEffectFn1($foreign["_abort"]);
export {
    toEventTarget,
    xmlHttpRequest,
    abort,
    getAllResponseHeaders,
    getResponseHeader,
    open,
    open$prime,
    overrideMimeType,
    send,
    sendString,
    sendDocument,
    sendBlob,
    sendArrayView,
    sendFormData,
    setRequestHeader,
    readyState,
    response,
    responseURL,
    status,
    statusText,
    timeout,
    setTimeout,
    upload,
    withCredentials,
    setWithCredentials
};
//# sourceMappingURL=index.js.map
