// Generated by purs version 0.15.10
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_MediaType_Common from "../Data.MediaType.Common/index.js";
var ArrayView = /* #__PURE__ */ (function () {
    function ArrayView(value0) {
        this.value0 = value0;
    };
    ArrayView.create = function (value0) {
        return new ArrayView(value0);
    };
    return ArrayView;
})();
var Blob = /* #__PURE__ */ (function () {
    function Blob(value0) {
        this.value0 = value0;
    };
    Blob.create = function (value0) {
        return new Blob(value0);
    };
    return Blob;
})();
var Document = /* #__PURE__ */ (function () {
    function Document(value0) {
        this.value0 = value0;
    };
    Document.create = function (value0) {
        return new Document(value0);
    };
    return Document;
})();
var $$String = /* #__PURE__ */ (function () {
    function $$String(value0) {
        this.value0 = value0;
    };
    $$String.create = function (value0) {
        return new $$String(value0);
    };
    return $$String;
})();
var FormData = /* #__PURE__ */ (function () {
    function FormData(value0) {
        this.value0 = value0;
    };
    FormData.create = function (value0) {
        return new FormData(value0);
    };
    return FormData;
})();
var FormURLEncoded = /* #__PURE__ */ (function () {
    function FormURLEncoded(value0) {
        this.value0 = value0;
    };
    FormURLEncoded.create = function (value0) {
        return new FormURLEncoded(value0);
    };
    return FormURLEncoded;
})();
var Json = /* #__PURE__ */ (function () {
    function Json(value0) {
        this.value0 = value0;
    };
    Json.create = function (value0) {
        return new Json(value0);
    };
    return Json;
})();
var toMediaType = function (v) {
    if (v instanceof FormURLEncoded) {
        return new Data_Maybe.Just(Data_MediaType_Common.applicationFormURLEncoded);
    };
    if (v instanceof Json) {
        return new Data_Maybe.Just(Data_MediaType_Common.applicationJSON);
    };
    return Data_Maybe.Nothing.value;
};
var string = /* #__PURE__ */ (function () {
    return $$String.create;
})();
var json = /* #__PURE__ */ (function () {
    return Json.create;
})();
var formURLEncoded = /* #__PURE__ */ (function () {
    return FormURLEncoded.create;
})();
var formData = /* #__PURE__ */ (function () {
    return FormData.create;
})();
var document = /* #__PURE__ */ (function () {
    return Document.create;
})();
var blob = /* #__PURE__ */ (function () {
    return Blob.create;
})();
var arrayView = function (av) {
    return new ArrayView(function (f) {
        return f(av);
    });
};
export {
    ArrayView,
    Blob,
    Document,
    $$String as String,
    FormData,
    FormURLEncoded,
    Json,
    arrayView,
    blob,
    document,
    string,
    formData,
    formURLEncoded,
    json,
    toMediaType
};
//# sourceMappingURL=index.js.map
