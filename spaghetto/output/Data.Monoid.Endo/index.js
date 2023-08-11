// Generated by purs version 0.15.10
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
import * as Data_Show from "../Data.Show/index.js";
var Endo = function (x) {
    return x;
};
var showEndo = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return {
        show: function (v) {
            return "(Endo " + (show(v) + ")");
        }
    };
};
var semigroupEndo = function (dictSemigroupoid) {
    var compose = Control_Semigroupoid.compose(dictSemigroupoid);
    return {
        append: function (v) {
            return function (v1) {
                return compose(v)(v1);
            };
        }
    };
};
var ordEndo = function (dictOrd) {
    return dictOrd;
};
var monoidEndo = function (dictCategory) {
    var semigroupEndo1 = semigroupEndo(dictCategory.Semigroupoid0());
    return {
        mempty: Control_Category.identity(dictCategory),
        Semigroup0: function () {
            return semigroupEndo1;
        }
    };
};
var eqEndo = function (dictEq) {
    return dictEq;
};
var boundedEndo = function (dictBounded) {
    return dictBounded;
};
export {
    Endo,
    eqEndo,
    ordEndo,
    boundedEndo,
    showEndo,
    semigroupEndo,
    monoidEndo
};
//# sourceMappingURL=index.js.map
