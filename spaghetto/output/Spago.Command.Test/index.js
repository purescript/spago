// Generated by purs version 0.15.10
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Registry_PackageName from "../Registry.PackageName/index.js";
import * as Spago_Command_Run from "../Spago.Command.Run/index.js";
import * as Spago_Core_Prelude from "../Spago.Core.Prelude/index.js";
import * as Spago_Log from "../Spago.Log/index.js";
import * as Spago_Paths from "../Spago.Paths/index.js";
var $$void = /* #__PURE__ */ Data_Functor["void"](Spago_Core_Prelude.functorSpago);
var $$for = /* #__PURE__ */ Data_Traversable["for"](Spago_Core_Prelude.applicativeSpago)(Data_Array_NonEmpty_Internal.traversableNonEmptyArray);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Spago_Core_Prelude.bindSpago);
var logInfo = /* #__PURE__ */ Spago_Log.logInfo(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago)(Spago_Log.loggableString);
var runSpago = /* #__PURE__ */ Spago_Core_Prelude.runSpago(Spago_Core_Prelude.monadAffSpago);
var run = /* #__PURE__ */ Control_Bind.bind(Spago_Core_Prelude.bindSpago)(/* #__PURE__ */ Control_Monad_Reader_Class.ask(Spago_Core_Prelude.monadAskSpago))(function (v) {
    return $$void($$for(v.selectedPackages)(function (v1) {
        var runOptions = {
            successMessage: new Data_Maybe.Just("Test succeeded for package \"" + (Registry_PackageName.print(v1["selected"]["package"].name) + "\".")),
            failureMessage: "Tests failed for package \"" + (Registry_PackageName.print(v1["selected"]["package"].name) + "\"."),
            executeDir: v1.selected.path,
            sourceDir: Spago_Paths.cwd,
            execArgs: v1.execArgs,
            moduleName: v1.moduleName
        };
        var runEnv = {
            logOptions: v.logOptions,
            workspace: v.workspace,
            selected: v1.selected,
            node: v.node,
            runOptions: runOptions,
            dependencies: v.dependencies,
            purs: v.purs
        };
        return discard(logInfo("Running tests for package: " + Registry_PackageName.print(v1["selected"]["package"].name)))(function () {
            return runSpago(runEnv)(Spago_Command_Run.run);
        });
    }));
});
export {
    run
};
