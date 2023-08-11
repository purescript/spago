// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Gen from "../Control.Monad.Gen/index.js";
import * as Control_Monad_Gen_Class from "../Control.Monad.Gen.Class/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var genMap = function (dictMonadRec) {
    var unfoldable = Control_Monad_Gen.unfoldable(dictMonadRec);
    return function (dictMonadGen) {
        var sized = Control_Monad_Gen_Class.sized(dictMonadGen);
        var Bind1 = (dictMonadGen.Monad0()).Bind1();
        var bind = Control_Bind.bind(Bind1);
        var chooseInt = Control_Monad_Gen_Class.chooseInt(dictMonadGen);
        var resize = Control_Monad_Gen_Class.resize(dictMonadGen);
        var Apply0 = Bind1.Apply0();
        var map = Data_Functor.map(Apply0.Functor0());
        var unfoldable1 = unfoldable(dictMonadGen)(Data_List_Types.unfoldableList);
        var apply = Control_Apply.apply(Apply0);
        return function (dictOrd) {
            var fromFoldable = Data_Map_Internal.fromFoldable(dictOrd)(Data_List_Types.foldableList);
            return function (genKey) {
                return function (genValue) {
                    return sized(function (size) {
                        return bind(chooseInt(0)(size))(function (newSize) {
                            return resize(Data_Function["const"](newSize))(map(fromFoldable)(unfoldable1(apply(map(Data_Tuple.Tuple.create)(genKey))(genValue))));
                        });
                    });
                };
            };
        };
    };
};
export {
    genMap
};
//# sourceMappingURL=index.js.map
