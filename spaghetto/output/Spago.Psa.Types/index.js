// Generated by purs version 0.15.10
import * as Data_Codec_Argonaut from "../Data.Codec.Argonaut/index.js";
import * as Data_Codec_Argonaut_Compat from "../Data.Codec.Argonaut.Compat/index.js";
import * as Data_Codec_Argonaut_Record from "../Data.Codec.Argonaut.Record/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordString);
var object = /* #__PURE__ */ Data_Codec_Argonaut_Record.object();
var rowListCodecCons = /* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(Data_Codec_Argonaut_Record.rowListCodecNil)()();
var compare1 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Tuple.ordTuple(Data_Ord.ordInt)(Data_Ord.ordInt));
var Unknown = /* #__PURE__ */ (function () {
    function Unknown() {

    };
    Unknown.value = new Unknown();
    return Unknown;
})();
var Lib = /* #__PURE__ */ (function () {
    function Lib(value0) {
        this.value0 = value0;
    };
    Lib.create = function (value0) {
        return new Lib(value0);
    };
    return Lib;
})();
var Src = /* #__PURE__ */ (function () {
    function Src(value0) {
        this.value0 = value0;
    };
    Src.create = function (value0) {
        return new Src(value0);
    };
    return Src;
})();
var eqPsaPath = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Unknown && y instanceof Unknown) {
                return true;
            };
            if (x instanceof Lib && y instanceof Lib) {
                return x.value0 === y.value0;
            };
            if (x instanceof Src && y instanceof Src) {
                return x.value0 === y.value0;
            };
            return false;
        };
    }
};
var ordPsaPath = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Unknown && y instanceof Unknown) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Unknown) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Unknown) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Lib && y instanceof Lib) {
                return compare(x.value0)(y.value0);
            };
            if (x instanceof Lib) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Lib) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Src && y instanceof Src) {
                return compare(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Spago.Psa.Types (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqPsaPath;
    }
};
var compare2 = /* #__PURE__ */ Data_Ord.compare(ordPsaPath);
var positionCodec = /* #__PURE__ */ object(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "startLine";
    }
}))()()({
    reflectSymbol: function () {
        return "startColumn";
    }
}))()()({
    reflectSymbol: function () {
        return "endLine";
    }
}))()()({
    reflectSymbol: function () {
        return "endColumn";
    }
}))("Position")({
    startLine: Data_Codec_Argonaut["int"],
    startColumn: Data_Codec_Argonaut["int"],
    endLine: Data_Codec_Argonaut["int"],
    endColumn: Data_Codec_Argonaut["int"]
});
var suggestionCodec = /* #__PURE__ */ object(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "replacement";
    }
}))()()({
    reflectSymbol: function () {
        return "replaceRange";
    }
}))("Suggestion")({
    replacement: Data_Codec_Argonaut.string,
    replaceRange: /* #__PURE__ */ Data_Codec_Argonaut_Compat.maybe(positionCodec)
});
var psaErrorCodec = /* #__PURE__ */ object(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "suggestion";
    }
}))()()({
    reflectSymbol: function () {
        return "position";
    }
}))()()({
    reflectSymbol: function () {
        return "moduleName";
    }
}))()()({
    reflectSymbol: function () {
        return "message";
    }
}))()()({
    reflectSymbol: function () {
        return "filename";
    }
}))()()({
    reflectSymbol: function () {
        return "errorLink";
    }
}))()()({
    reflectSymbol: function () {
        return "errorCode";
    }
}))("PsaError")({
    moduleName: /* #__PURE__ */ Data_Codec_Argonaut_Compat.maybe(Data_Codec_Argonaut.string),
    errorCode: Data_Codec_Argonaut.string,
    errorLink: Data_Codec_Argonaut.string,
    message: Data_Codec_Argonaut.string,
    filename: /* #__PURE__ */ Data_Codec_Argonaut_Compat.maybe(Data_Codec_Argonaut.string),
    position: /* #__PURE__ */ Data_Codec_Argonaut_Compat.maybe(positionCodec),
    suggestion: /* #__PURE__ */ Data_Codec_Argonaut_Compat.maybe(suggestionCodec)
});
var psaResultCodec = /* #__PURE__ */ object(/* #__PURE__ */ Data_Codec_Argonaut_Record.rowListCodecCons(/* #__PURE__ */ rowListCodecCons({
    reflectSymbol: function () {
        return "warnings";
    }
}))()()({
    reflectSymbol: function () {
        return "errors";
    }
}))("PsaResult")({
    warnings: /* #__PURE__ */ Data_Codec_Argonaut.array(psaErrorCodec),
    errors: /* #__PURE__ */ Data_Codec_Argonaut.array(psaErrorCodec)
});
var compareByLocation = function (err1) {
    return function (err2) {
        var v = compare2(err1.path)(err2.path);
        if (v instanceof Data_Ordering.EQ) {
            if (err1.position instanceof Data_Maybe.Nothing && err2.position instanceof Data_Maybe.Nothing) {
                return Data_Ordering.EQ.value;
            };
            if (err1.position instanceof Data_Maybe.Nothing) {
                return Data_Ordering.LT.value;
            };
            if (err2.position instanceof Data_Maybe.Nothing) {
                return Data_Ordering.GT.value;
            };
            if (err1.position instanceof Data_Maybe.Just && err2.position instanceof Data_Maybe.Just) {
                return compare1(new Data_Tuple.Tuple(err1.position.value0.startLine, err1.position.value0.startColumn))(new Data_Tuple.Tuple(err2.position.value0.startLine, err2.position.value0.startColumn));
            };
            throw new Error("Failed pattern match at Spago.Psa.Types (line 101, column 7 - line 107, column 46): " + [ err1.position.constructor.name, err2.position.constructor.name ]);
        };
        return v;
    };
};
export {
    Unknown,
    Lib,
    Src,
    psaResultCodec,
    psaErrorCodec,
    compareByLocation,
    eqPsaPath,
    ordPsaPath
};
//# sourceMappingURL=index.js.map
