import * as $foreign from "./foreign.js";
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Codec_Argonaut_Common from "../Data.Codec.Argonaut.Common/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Filterable from "../Data.Filterable/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_JSDate from "../Data.JSDate/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Set_NonEmpty from "../Data.Set.NonEmpty/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Dodo from "../Dodo/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Aff_Class from "../Effect.Aff.Class/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
import * as Node_Path from "../Node.Path/index.js";
import * as Node_Process from "../Node.Process/index.js";
import * as Options_Applicative_Builder from "../Options.Applicative.Builder/index.js";
import * as Options_Applicative_Builder_Internal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options_Applicative_Extra from "../Options.Applicative.Extra/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as Optparse from "../Optparse/index.js";
import * as Record from "../Record/index.js";
import * as Registry_Constants from "../Registry.Constants/index.js";
import * as Registry_ManifestIndex from "../Registry.ManifestIndex/index.js";
import * as Registry_Metadata from "../Registry.Metadata/index.js";
import * as Registry_PackageName from "../Registry.PackageName/index.js";
import * as Registry_Version from "../Registry.Version/index.js";
import * as Spago_Bin_Flags from "../Spago.Bin.Flags/index.js";
import * as Spago_Command_Build from "../Spago.Command.Build/index.js";
import * as Spago_Command_Bundle from "../Spago.Command.Bundle/index.js";
import * as Spago_Command_Fetch from "../Spago.Command.Fetch/index.js";
import * as Spago_Command_Init from "../Spago.Command.Init/index.js";
import * as Spago_Command_Ls from "../Spago.Command.Ls/index.js";
import * as Spago_Command_Publish from "../Spago.Command.Publish/index.js";
import * as Spago_Command_Registry from "../Spago.Command.Registry/index.js";
import * as Spago_Command_Repl from "../Spago.Command.Repl/index.js";
import * as Spago_Command_Run from "../Spago.Command.Run/index.js";
import * as Spago_Command_Sources from "../Spago.Command.Sources/index.js";
import * as Spago_Command_Test from "../Spago.Command.Test/index.js";
import * as Spago_Config from "../Spago.Config/index.js";
import * as Spago_Core_Config from "../Spago.Core.Config/index.js";
import * as Spago_Core_Prelude from "../Spago.Core.Prelude/index.js";
import * as Spago_Esbuild from "../Spago.Esbuild/index.js";
import * as Spago_FS from "../Spago.FS/index.js";
import * as Spago_Generated_BuildInfo from "../Spago.Generated.BuildInfo/index.js";
import * as Spago_Git from "../Spago.Git/index.js";
import * as Spago_Json from "../Spago.Json/index.js";
import * as Spago_Log from "../Spago.Log/index.js";
import * as Spago_Paths from "../Spago.Paths/index.js";
import * as Spago_Prelude from "../Spago.Prelude/index.js";
import * as Spago_Purs from "../Spago.Purs/index.js";
var fromRecord = /* #__PURE__ */ Optparse.fromRecord();
var backendArgsIsSymbol = {
    reflectSymbol: function () {
        return "backendArgs";
    }
};
var buildArgsCons = /* #__PURE__ */ Optparse.buildArgsCons(backendArgsIsSymbol)()()();
var censorBuildWarningsIsSymbol = {
    reflectSymbol: function () {
        return "censorBuildWarnings";
    }
};
var buildArgsCons1 = /* #__PURE__ */ Optparse.buildArgsCons(censorBuildWarningsIsSymbol)()()();
var censorCodesIsSymbol = {
    reflectSymbol: function () {
        return "censorCodes";
    }
};
var buildArgsCons2 = /* #__PURE__ */ Optparse.buildArgsCons(censorCodesIsSymbol)()()();
var execArgsIsSymbol = {
    reflectSymbol: function () {
        return "execArgs";
    }
};
var buildArgsCons3 = /* #__PURE__ */ Optparse.buildArgsCons(execArgsIsSymbol)()()();
var filterCodesIsSymbol = {
    reflectSymbol: function () {
        return "filterCodes";
    }
};
var buildArgsCons4 = /* #__PURE__ */ Optparse.buildArgsCons(filterCodesIsSymbol)()()();
var outputIsSymbol = {
    reflectSymbol: function () {
        return "output";
    }
};
var buildArgsCons5 = /* #__PURE__ */ Optparse.buildArgsCons(outputIsSymbol)()()();
var pedanticPackagesIsSymbol = {
    reflectSymbol: function () {
        return "pedanticPackages";
    }
};
var buildArgsCons6 = /* #__PURE__ */ Optparse.buildArgsCons(pedanticPackagesIsSymbol)()()();
var persistWarningsIsSymbol = {
    reflectSymbol: function () {
        return "persistWarnings";
    }
};
var buildArgsCons7 = /* #__PURE__ */ Optparse.buildArgsCons(persistWarningsIsSymbol)()()();
var pursArgsIsSymbol = {
    reflectSymbol: function () {
        return "pursArgs";
    }
};
var buildArgsCons8 = /* #__PURE__ */ Optparse.buildArgsCons(pursArgsIsSymbol)()()();
var selectedPackageIsSymbol = {
    reflectSymbol: function () {
        return "selectedPackage";
    }
};
var buildArgsCons9 = /* #__PURE__ */ Optparse.buildArgsCons(selectedPackageIsSymbol)()()();
var showSourceIsSymbol = {
    reflectSymbol: function () {
        return "showSource";
    }
};
var buildArgsCons10 = /* #__PURE__ */ Optparse.buildArgsCons(showSourceIsSymbol)()()();
var statVerbosityIsSymbol = {
    reflectSymbol: function () {
        return "statVerbosity";
    }
};
var buildArgsCons11 = /* #__PURE__ */ Optparse.buildArgsCons(statVerbosityIsSymbol)()()();
var strictIsSymbol = {
    reflectSymbol: function () {
        return "strict";
    }
};
var buildArgsCons12 = /* #__PURE__ */ Optparse.buildArgsCons(strictIsSymbol)()()();
var buildArgsCons13 = /* #__PURE__ */ buildArgsCons5(/* #__PURE__ */ buildArgsCons6(/* #__PURE__ */ buildArgsCons7(/* #__PURE__ */ buildArgsCons8(/* #__PURE__ */ buildArgsCons9(/* #__PURE__ */ buildArgsCons10(/* #__PURE__ */ buildArgsCons11(/* #__PURE__ */ buildArgsCons12(Optparse.buildRecordArgsNil))))))));
var buildArgsCons14 = /* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "json";
    }
})()()();
var buildArgsCons15 = /* #__PURE__ */ buildArgsCons9(Optparse.buildRecordArgsNil);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Spago_Core_Prelude.bindSpago);
var ensureFileSync = /* #__PURE__ */ Spago_FS.ensureFileSync(Spago_Core_Prelude.monadEffectSpago);
var writeTextFile = /* #__PURE__ */ Spago_FS.writeTextFile(Spago_Core_Prelude.monadAffSpago);
var bind = /* #__PURE__ */ Control_Bind.bind(Spago_Core_Prelude.bindSpago);
var logDebug = /* #__PURE__ */ Spago_Log.logDebug(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago);
var loggableArray = /* #__PURE__ */ Spago_Log.loggableArray(Spago_Log.loggableString);
var logDebug1 = /* #__PURE__ */ logDebug(loggableArray);
var show = /* #__PURE__ */ Data_Show.show(Effect_Exception.showError);
var pure = /* #__PURE__ */ Control_Applicative.pure(Spago_Core_Prelude.applicativeSpago);
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(Spago_Core_Prelude.monadEffectSpago);
var logDebug2 = /* #__PURE__ */ logDebug(Spago_Log.loggableString);
var ensureRangesIsSymbol = {
    reflectSymbol: function () {
        return "ensureRanges";
    }
};
var buildArgsCons16 = /* #__PURE__ */ Optparse.buildArgsCons(ensureRangesIsSymbol)()()();
var mainIsSymbol = {
    reflectSymbol: function () {
        return "main";
    }
};
var buildArgsCons17 = /* #__PURE__ */ buildArgsCons8(buildArgsCons15);
var buildArgsCons18 = /* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "package";
    }
})()()()(Optparse.buildRecordArgsNil);
var ask = /* #__PURE__ */ Control_Monad_Reader_Class.ask(Spago_Core_Prelude.monadAskSpago);
var showRecord = /* #__PURE__ */ Data_Show.showRecord()();
var showRecordFieldsCons = /* #__PURE__ */ Data_Show.showRecordFieldsCons(backendArgsIsSymbol);
var showRecordFieldsCons1 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(censorBuildWarningsIsSymbol);
var showRecordFieldsCons2 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(censorCodesIsSymbol);
var showRecordFieldsCons3 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(execArgsIsSymbol);
var showRecordFieldsCons4 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(filterCodesIsSymbol);
var showRecordFieldsCons5 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(outputIsSymbol);
var showRecordFieldsCons6 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(pedanticPackagesIsSymbol);
var showRecordFieldsCons7 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(persistWarningsIsSymbol);
var showRecordFieldsCons8 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(pursArgsIsSymbol);
var showRecordFieldsCons9 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(selectedPackageIsSymbol);
var showRecordFieldsCons10 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(showSourceIsSymbol);
var showRecordFieldsCons11 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(statVerbosityIsSymbol);
var showMaybe = /* #__PURE__ */ Data_Maybe.showMaybe(Data_Show.showBoolean);
var showMaybe1 = /* #__PURE__ */ Data_Maybe.showMaybe(Spago_Core_Config.showStatVerbosity);
var showMaybe2 = /* #__PURE__ */ Data_Maybe.showMaybe(Spago_Core_Config.showShowSourceCode);
var showMaybe3 = /* #__PURE__ */ Data_Maybe.showMaybe(Data_Show.showString);
var showList = /* #__PURE__ */ Data_List_Types.showList(Data_Show.showString);
var showRecordFieldsCons12 = /* #__PURE__ */ showRecordFieldsCons5(/* #__PURE__ */ showRecordFieldsCons6(/* #__PURE__ */ showRecordFieldsCons7(/* #__PURE__ */ showRecordFieldsCons8(/* #__PURE__ */ showRecordFieldsCons9(/* #__PURE__ */ showRecordFieldsCons10(/* #__PURE__ */ showRecordFieldsCons11(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil(strictIsSymbol)(showMaybe))(showMaybe1))(showMaybe2))(showMaybe3))(showList))(showMaybe))(Data_Show.showBoolean))(showMaybe3);
var showMaybe4 = /* #__PURE__ */ Data_Maybe.showMaybe(/* #__PURE__ */ Data_Set_NonEmpty.showNonEmptySet(Data_Show.showString));
var showMaybe5 = /* #__PURE__ */ Data_Maybe.showMaybe(/* #__PURE__ */ Data_Show.showArray(Data_Show.showString));
var showMaybe6 = /* #__PURE__ */ Data_Maybe.showMaybe(Spago_Core_Config.showCensorBuildWarnings);
var show1 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ showRecord(/* #__PURE__ */ showRecordFieldsCons(/* #__PURE__ */ showRecordFieldsCons1(/* #__PURE__ */ showRecordFieldsCons2(/* #__PURE__ */ showRecordFieldsCons3(/* #__PURE__ */ showRecordFieldsCons4(showRecordFieldsCons12)(showMaybe4))(showMaybe5))(showMaybe4))(showMaybe6))(showList)));
var bind1 = /* #__PURE__ */ Control_Bind.bind(Data_Maybe.bindMaybe);
var alt = /* #__PURE__ */ Control_Alt.alt(Data_Maybe.altMaybe);
var map = /* #__PURE__ */ Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray);
var die = /* #__PURE__ */ Spago_Log.die(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago);
var die1 = /* #__PURE__ */ die(Spago_Log.loggableString);
var showRecordFieldsCons13 = /* #__PURE__ */ Data_Show.showRecordFieldsCons(ensureRangesIsSymbol);
var show2 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ showRecord(/* #__PURE__ */ showRecordFieldsCons(/* #__PURE__ */ showRecordFieldsCons1(/* #__PURE__ */ showRecordFieldsCons2(/* #__PURE__ */ showRecordFieldsCons13(/* #__PURE__ */ showRecordFieldsCons3(/* #__PURE__ */ showRecordFieldsCons4(/* #__PURE__ */ Data_Show.showRecordFieldsCons(mainIsSymbol)(showRecordFieldsCons12)(showMaybe3))(showMaybe4))(showMaybe5))(Data_Show.showBoolean))(showMaybe4))(showMaybe6))(showList)));
var die2 = /* #__PURE__ */ die(/* #__PURE__ */ Spago_Log.loggableArray(Spago_Log.loggableDocc));
var toDoc = /* #__PURE__ */ Spago_Log.toDoc(Spago_Log.loggableString);
var toDoc1 = /* #__PURE__ */ Spago_Log.toDoc(/* #__PURE__ */ Spago_Log.loggableArray(Spago_Log.loggablePackageName));
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var show3 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ showRecord(/* #__PURE__ */ showRecordFieldsCons(/* #__PURE__ */ showRecordFieldsCons8(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil(selectedPackageIsSymbol)(showMaybe3))(showList))(showList)));
var fromFoldable = /* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList);
var mkdirp = /* #__PURE__ */ Spago_FS.mkdirp(Spago_Core_Prelude.monadAffSpago);
var show4 = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var lookup = /* #__PURE__ */ Data_Map_Internal.lookup(Registry_PackageName.ordPackageName);
var lookup1 = /* #__PURE__ */ Data_Map_Internal.lookup(Registry_Version.ordVersion);
var liftAff = /* #__PURE__ */ Effect_Aff_Class.liftAff(Spago_Core_Prelude.monadAffSpago);
var readEntryFile = /* #__PURE__ */ Registry_ManifestIndex.readEntryFile(Effect_Aff_Class.monadAffAff);
var map2 = /* #__PURE__ */ Data_Functor.map(Spago_Core_Prelude.functorSpago);
var toUnfoldable = /* #__PURE__ */ Data_Array_NonEmpty.toUnfoldable(Data_Unfoldable.unfoldableArray);
var logWarn = /* #__PURE__ */ Spago_Log.logWarn(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago)(Spago_Log.loggableString);
var fromFoldable1 = /* #__PURE__ */ Data_Map_Internal.fromFoldable(Registry_Version.ordVersion)(Data_Foldable.foldableArray);
var insert = /* #__PURE__ */ Data_Map_Internal.insert(Registry_PackageName.ordPackageName);
var whenM = /* #__PURE__ */ Control_Monad.whenM(Spago_Core_Prelude.monadSpago);
var logInfo = /* #__PURE__ */ Spago_Log.logInfo(Spago_Core_Prelude.monadEffectSpago)(Spago_Core_Prelude.monadAskSpago)(Spago_Log.loggableString);
var runSpago = /* #__PURE__ */ Spago_Core_Prelude.runSpago(Spago_Core_Prelude.monadAffSpago);
var $$try = /* #__PURE__ */ Control_Monad_Error_Class["try"](Spago_Core_Prelude.monadErrorErrorSpago);
var union = /* #__PURE__ */ Record.union();
var bind2 = /* #__PURE__ */ Control_Bind.bind(Effect_Aff.bindAff);
var liftEffect1 = /* #__PURE__ */ Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var and = /* #__PURE__ */ Data_Foldable.and(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Effect_Aff.applicativeAff);
var partitionMap = /* #__PURE__ */ Data_Filterable.partitionMap(Data_Filterable.filterableArray);
var unless = /* #__PURE__ */ Control_Applicative.unless(Spago_Core_Prelude.applicativeSpago);
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var $$for = /* #__PURE__ */ Data_Traversable["for"](Spago_Core_Prelude.applicativeSpago)(Data_Traversable.traversableMaybe);
var show5 = /* #__PURE__ */ Data_Show.show(showMaybe3);
var minifyIsSymbol = {
    reflectSymbol: function () {
        return "minify";
    }
};
var moduleIsSymbol = {
    reflectSymbol: function () {
        return "module";
    }
};
var outfileIsSymbol = {
    reflectSymbol: function () {
        return "outfile";
    }
};
var platformIsSymbol = {
    reflectSymbol: function () {
        return "platform";
    }
};
var typeIsSymbol = {
    reflectSymbol: function () {
        return "type";
    }
};
var show6 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ showRecord(/* #__PURE__ */ showRecordFieldsCons(/* #__PURE__ */ showRecordFieldsCons1(/* #__PURE__ */ showRecordFieldsCons2(/* #__PURE__ */ showRecordFieldsCons13(/* #__PURE__ */ showRecordFieldsCons4(/* #__PURE__ */ Data_Show.showRecordFieldsCons(minifyIsSymbol)(/* #__PURE__ */ Data_Show.showRecordFieldsCons(moduleIsSymbol)(/* #__PURE__ */ Data_Show.showRecordFieldsCons(outfileIsSymbol)(/* #__PURE__ */ showRecordFieldsCons5(/* #__PURE__ */ showRecordFieldsCons6(/* #__PURE__ */ showRecordFieldsCons7(/* #__PURE__ */ Data_Show.showRecordFieldsCons(platformIsSymbol)(/* #__PURE__ */ showRecordFieldsCons8(/* #__PURE__ */ showRecordFieldsCons9(/* #__PURE__ */ showRecordFieldsCons10(/* #__PURE__ */ showRecordFieldsCons11(/* #__PURE__ */ Data_Show.showRecordFieldsCons(strictIsSymbol)(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil(typeIsSymbol)(showMaybe3))(showMaybe))(showMaybe1))(showMaybe2))(showMaybe3))(showList))(showMaybe3))(showMaybe))(Data_Show.showBoolean))(showMaybe3))(showMaybe3))(showMaybe3))(Data_Show.showBoolean))(showMaybe4))(Data_Show.showBoolean))(showMaybe4))(showMaybe6))(showList)));
var bindFlipped = /* #__PURE__ */ Control_Bind.bindFlipped(Data_Maybe.bindMaybe);
var map3 = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var buildArgsCons19 = /* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "packages";
    }
})()()();
var apply = /* #__PURE__ */ Control_Apply.apply(Options_Applicative_Types.parserApply);
var map4 = /* #__PURE__ */ Data_Functor.map(Options_Applicative_Types.parserFunctor);
var fold = /* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableArray)(Options_Applicative_Builder_Internal.modMonoid);
var append2 = /* #__PURE__ */ Data_Semigroup.append(Options_Applicative_Builder_Internal.modSemigroup);
var runSpago1 = /* #__PURE__ */ Spago_Core_Prelude.runSpago(Effect_Aff_Class.monadAffAff);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Data_List_Types.monoidList);
var $$void = /* #__PURE__ */ Data_Functor["void"](Spago_Core_Prelude.functorSpago);
var die3 = /* #__PURE__ */ die(loggableArray);
var merge = /* #__PURE__ */ Record.merge()();
var toUnfoldable1 = /* #__PURE__ */ Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray);
var mempty1 = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Data_Maybe.monoidMaybe(Data_Semigroup.semigroupString));
var exists = /* #__PURE__ */ Spago_FS.exists(Spago_Core_Prelude.monadEffectSpago);
var mkTemp = /* #__PURE__ */ Spago_Prelude.mkTemp(Spago_Core_Prelude.monadAffSpago);
var fromFoldable2 = /* #__PURE__ */ Data_List.fromFoldable(Data_Foldable.foldableArray);
var union1 = /* #__PURE__ */ Data_Map_Internal.union(Registry_PackageName.ordPackageName);
var when = /* #__PURE__ */ Control_Applicative.when(Effect_Aff.applicativeAff);
var Init = /* #__PURE__ */ (function () {
    function Init(value0) {
        this.value0 = value0;
    };
    Init.create = function (value0) {
        return new Init(value0);
    };
    return Init;
})();
var Fetch = /* #__PURE__ */ (function () {
    function Fetch(value0) {
        this.value0 = value0;
    };
    Fetch.create = function (value0) {
        return new Fetch(value0);
    };
    return Fetch;
})();
var Install = /* #__PURE__ */ (function () {
    function Install(value0) {
        this.value0 = value0;
    };
    Install.create = function (value0) {
        return new Install(value0);
    };
    return Install;
})();
var Build = /* #__PURE__ */ (function () {
    function Build(value0) {
        this.value0 = value0;
    };
    Build.create = function (value0) {
        return new Build(value0);
    };
    return Build;
})();
var Bundle = /* #__PURE__ */ (function () {
    function Bundle(value0) {
        this.value0 = value0;
    };
    Bundle.create = function (value0) {
        return new Bundle(value0);
    };
    return Bundle;
})();
var Repl = /* #__PURE__ */ (function () {
    function Repl(value0) {
        this.value0 = value0;
    };
    Repl.create = function (value0) {
        return new Repl(value0);
    };
    return Repl;
})();
var Run = /* #__PURE__ */ (function () {
    function Run(value0) {
        this.value0 = value0;
    };
    Run.create = function (value0) {
        return new Run(value0);
    };
    return Run;
})();
var Test = /* #__PURE__ */ (function () {
    function Test(value0) {
        this.value0 = value0;
    };
    Test.create = function (value0) {
        return new Test(value0);
    };
    return Test;
})();
var Sources = /* #__PURE__ */ (function () {
    function Sources(value0) {
        this.value0 = value0;
    };
    Sources.create = function (value0) {
        return new Sources(value0);
    };
    return Sources;
})();
var RegistrySearch = /* #__PURE__ */ (function () {
    function RegistrySearch(value0) {
        this.value0 = value0;
    };
    RegistrySearch.create = function (value0) {
        return new RegistrySearch(value0);
    };
    return RegistrySearch;
})();
var RegistryInfo = /* #__PURE__ */ (function () {
    function RegistryInfo(value0) {
        this.value0 = value0;
    };
    RegistryInfo.create = function (value0) {
        return new RegistryInfo(value0);
    };
    return RegistryInfo;
})();
var LsDeps = /* #__PURE__ */ (function () {
    function LsDeps(value0) {
        this.value0 = value0;
    };
    LsDeps.create = function (value0) {
        return new LsDeps(value0);
    };
    return LsDeps;
})();
var LsPackages = /* #__PURE__ */ (function () {
    function LsPackages(value0) {
        this.value0 = value0;
    };
    LsPackages.create = function (value0) {
        return new LsPackages(value0);
    };
    return LsPackages;
})();
var Publish = /* #__PURE__ */ (function () {
    function Publish(value0) {
        this.value0 = value0;
    };
    Publish.create = function (value0) {
        return new Publish(value0);
    };
    return Publish;
})();
var SpagoCmd = /* #__PURE__ */ (function () {
    function SpagoCmd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SpagoCmd.create = function (value0) {
        return function (value1) {
            return new SpagoCmd(value0, value1);
        };
    };
    return SpagoCmd;
})();
var testArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons(/* #__PURE__ */ buildArgsCons1(/* #__PURE__ */ buildArgsCons2(/* #__PURE__ */ buildArgsCons3(/* #__PURE__ */ buildArgsCons4(buildArgsCons13))))))({
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    pursArgs: Spago_Bin_Flags.pursArgs,
    backendArgs: Spago_Bin_Flags.backendArgs,
    execArgs: Spago_Bin_Flags.execArgs,
    output: Spago_Bin_Flags.output,
    pedanticPackages: Spago_Bin_Flags.pedanticPackages,
    strict: Spago_Bin_Flags.strict,
    censorBuildWarnings: Spago_Bin_Flags.censorBuildWarnings,
    showSource: Spago_Bin_Flags.showSource,
    censorCodes: Spago_Bin_Flags.censorCodes,
    filterCodes: Spago_Bin_Flags.filterCodes,
    statVerbosity: Spago_Bin_Flags.statVerbosity,
    persistWarnings: Spago_Bin_Flags.persistWarnings
});
var sourcesArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons14(buildArgsCons15))({
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    json: Spago_Bin_Flags.json
});
var shouldFetchRegistryRepos = /* #__PURE__ */ (function () {
    var touch = function (path) {
        return discard(ensureFileSync(path))(function () {
            return writeTextFile(path)("");
        });
    };
    var freshRegistryCanary = Node_Path.concat([ Spago_Paths.globalCachePath, "fresh-registry-canary.txt" ]);
    return bind(Spago_FS.stat(Spago_Core_Prelude.monadAffSpago)(freshRegistryCanary))(function (v) {
        if (v instanceof Data_Either.Left) {
            return discard(logDebug1([ "Could not stat " + freshRegistryCanary, show(v.value0) ]))(function () {
                return discard(touch(freshRegistryCanary))(function () {
                    return pure(true);
                });
            });
        };
        if (v instanceof Data_Either.Right) {
            return bind(liftEffect(Data_JSDate.now))(function (now) {
                var staleAfter = 1000.0 * 60.0 * 15.0;
                var isOldEnough = Data_JSDate.getTime(now) > Data_JSDate.getTime(v.value0.value0.mtime) + staleAfter;
                if (isOldEnough) {
                    return discard(logDebug2("Registry is old enough, refreshing canary"))(function () {
                        return discard(touch(freshRegistryCanary))(function () {
                            return pure(true);
                        });
                    });
                };
                return discard(logDebug2("Registry index is fresh enough, moving on..."))(function () {
                    return pure(false);
                });
            });
        };
        throw new Error("Failed pattern match at Main (line 939, column 35 - line 958, column 19): " + [ v.constructor.name ]);
    });
})();
var runArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons(/* #__PURE__ */ buildArgsCons1(/* #__PURE__ */ buildArgsCons2(/* #__PURE__ */ buildArgsCons16(/* #__PURE__ */ buildArgsCons3(/* #__PURE__ */ buildArgsCons4(/* #__PURE__ */ Optparse.buildArgsCons(mainIsSymbol)()()()(buildArgsCons13))))))))({
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    pursArgs: Spago_Bin_Flags.pursArgs,
    backendArgs: Spago_Bin_Flags.backendArgs,
    execArgs: Spago_Bin_Flags.execArgs,
    output: Spago_Bin_Flags.output,
    pedanticPackages: Spago_Bin_Flags.pedanticPackages,
    main: Spago_Bin_Flags.moduleName,
    ensureRanges: Spago_Bin_Flags.ensureRanges,
    strict: Spago_Bin_Flags.strict,
    censorBuildWarnings: Spago_Bin_Flags.censorBuildWarnings,
    showSource: Spago_Bin_Flags.showSource,
    censorCodes: Spago_Bin_Flags.censorCodes,
    filterCodes: Spago_Bin_Flags.filterCodes,
    statVerbosity: Spago_Bin_Flags.statVerbosity,
    persistWarnings: Spago_Bin_Flags.persistWarnings
});
var replArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons(buildArgsCons17))({
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    pursArgs: Spago_Bin_Flags.pursArgs,
    backendArgs: Spago_Bin_Flags.backendArgs
});
var registrySearchArgsParser = /* #__PURE__ */ fromRecord(buildArgsCons18)({
    "package": Spago_Bin_Flags["package"]
});
var registryInfoArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "maybeVersion";
    }
})()()()(buildArgsCons18))({
    "package": Spago_Bin_Flags["package"],
    maybeVersion: Spago_Bin_Flags.maybeVersion
});
var publishArgsParser = /* #__PURE__ */ fromRecord(buildArgsCons15)({
    selectedPackage: Spago_Bin_Flags.selectedPackage
});
var mkTestEnv = function (testArgs) {
    return function (v) {
        return bind(ask)(function (v1) {
            return discard(logDebug2("Test args: " + show1(testArgs)))(function () {
                return bind(Spago_Command_Run.getNode)(function (node) {
                    var mkSelectedTest = function (selected) {
                        var testConf = function (f) {
                            return bind1(selected["package"].test)(f);
                        };
                        var moduleName = Data_Maybe.fromMaybe("Test.Main")(testConf(function ($682) {
                            return Data_Maybe.Just.create((function (v2) {
                                return v2.main;
                            })($682));
                        }));
                        var execArgs = Data_Maybe.fromMaybe([  ])(alt(testArgs.execArgs)(testConf(function (v2) {
                            return v2.execArgs;
                        })));
                        return {
                            moduleName: moduleName,
                            execArgs: execArgs,
                            selected: selected
                        };
                    };
                    return bind((function () {
                        if (v1.workspace.selected instanceof Data_Maybe.Just) {
                            return pure(Data_Array_NonEmpty.singleton(mkSelectedTest(v1.workspace.selected.value0)));
                        };
                        if (v1.workspace.selected instanceof Data_Maybe.Nothing) {
                            var workspacePackages = Spago_Config.getWorkspacePackages(v1.workspace.packageSet);
                            var v2 = Data_Array.uncons(Data_Array.filter(function (v3) {
                                return v3.hasTests;
                            })(workspacePackages));
                            if (v2 instanceof Data_Maybe.Just) {
                                return pure(map(mkSelectedTest)(Data_Array_NonEmpty["cons$prime"](v2.value0.head)(v2.value0.tail)));
                            };
                            if (v2 instanceof Data_Maybe.Nothing) {
                                return die1("No package found to test.");
                            };
                            throw new Error("Failed pattern match at Main (line 724, column 9 - line 726, column 53): " + [ v2.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Main (line 718, column 23 - line 726, column 53): " + [ v1.workspace.selected.constructor.name ]);
                    })())(function (selectedPackages) {
                        return discard(logDebug2("Selected packages to test: " + Spago_Json.stringifyJson(Data_Codec_Argonaut_Common.nonEmptyArray(Registry_PackageName.codec))(map(function (v2) {
                            return v2["selected"]["package"].name;
                        })(selectedPackages))))(function () {
                            var newWorkspace = {
                                buildOptions: {
                                    output: alt(testArgs.output)(v1.workspace.buildOptions.output),
                                    censorBuildWarnings: v1.workspace.buildOptions.censorBuildWarnings,
                                    censorCodes: v1.workspace.buildOptions.censorCodes,
                                    filterCodes: v1.workspace.buildOptions.filterCodes,
                                    pedanticPackages: v1.workspace.buildOptions.pedanticPackages,
                                    persistWarnings: v1.workspace.buildOptions.persistWarnings,
                                    showSource: v1.workspace.buildOptions.showSource,
                                    statVerbosity: v1.workspace.buildOptions.statVerbosity,
                                    strict: v1.workspace.buildOptions.strict
                                },
                                packageSet: v1.workspace.packageSet,
                                selected: v1.workspace.selected,
                                backend: v1.workspace.backend,
                                compatibleCompiler: v1.workspace.compatibleCompiler,
                                doc: v1.workspace.doc,
                                lockfile: v1.workspace.lockfile,
                                originalConfig: v1.workspace.originalConfig
                            };
                            var testEnv = {
                                logOptions: v1.logOptions,
                                workspace: newWorkspace,
                                selectedPackages: selectedPackages,
                                node: node,
                                dependencies: v.dependencies,
                                purs: v.purs
                            };
                            return pure(testEnv);
                        });
                    });
                });
            });
        });
    };
};
var mkRunEnv = function (runArgs) {
    return function (v) {
        return bind(ask)(function (v1) {
            return discard(logDebug2("Run args: " + show2(runArgs)))(function () {
                return bind(Spago_Command_Run.getNode)(function (node) {
                    return bind((function () {
                        if (v1.workspace.selected instanceof Data_Maybe.Just) {
                            return pure(v1.workspace.selected.value0);
                        };
                        if (v1.workspace.selected instanceof Data_Maybe.Nothing) {
                            var workspacePackages = Spago_Config.getWorkspacePackages(v1.workspace.packageSet);
                            if (workspacePackages.length === 1) {
                                return pure(workspacePackages[0]);
                            };
                            return discard(logDebug2(Spago_Prelude.unsafeStringify(workspacePackages)))(function () {
                                return die2([ toDoc("No package was selected for running. Please select (with -p) one of the following packages:"), Dodo.indent(toDoc1(map1(function (v2) {
                                    return v2["package"].name;
                                })(workspacePackages))) ]);
                            });
                        };
                        throw new Error("Failed pattern match at Main (line 655, column 15 - line 669, column 16): " + [ v1.workspace.selected.constructor.name ]);
                    })())(function (selected) {
                        return discard(logDebug2("Selected package to run: " + Registry_PackageName.print(selected["package"].name)))(function () {
                            var runConf = function (f) {
                                return bind1(selected["package"].run)(f);
                            };
                            var moduleName = Data_Maybe.fromMaybe("Main")(alt(runArgs.main)(runConf(function (v2) {
                                return v2.main;
                            })));
                            var execArgs = Data_Maybe.fromMaybe([  ])(alt(runArgs.execArgs)(runConf(function (v2) {
                                return v2.execArgs;
                            })));
                            var runOptions = {
                                moduleName: moduleName,
                                execArgs: execArgs,
                                sourceDir: Spago_Paths.cwd,
                                executeDir: Spago_Paths.cwd,
                                successMessage: Data_Maybe.Nothing.value,
                                failureMessage: "Running failed."
                            };
                            var newWorkspace = {
                                buildOptions: {
                                    output: alt(runArgs.output)(v1.workspace.buildOptions.output),
                                    censorBuildWarnings: v1.workspace.buildOptions.censorBuildWarnings,
                                    censorCodes: v1.workspace.buildOptions.censorCodes,
                                    filterCodes: v1.workspace.buildOptions.filterCodes,
                                    pedanticPackages: v1.workspace.buildOptions.pedanticPackages,
                                    persistWarnings: v1.workspace.buildOptions.persistWarnings,
                                    showSource: v1.workspace.buildOptions.showSource,
                                    statVerbosity: v1.workspace.buildOptions.statVerbosity,
                                    strict: v1.workspace.buildOptions.strict
                                },
                                packageSet: v1.workspace.packageSet,
                                selected: v1.workspace.selected,
                                backend: v1.workspace.backend,
                                compatibleCompiler: v1.workspace.compatibleCompiler,
                                doc: v1.workspace.doc,
                                lockfile: v1.workspace.lockfile,
                                originalConfig: v1.workspace.originalConfig
                            };
                            var runEnv = {
                                logOptions: v1.logOptions,
                                workspace: newWorkspace,
                                selected: selected,
                                node: node,
                                runOptions: runOptions,
                                dependencies: v.dependencies,
                                purs: v.purs
                            };
                            return pure(runEnv);
                        });
                    });
                });
            });
        });
    };
};
var mkReplEnv = function (replArgs) {
    return function (dependencies) {
        return bind(ask)(function (v) {
            return discard(logDebug2("Repl args: " + show3(replArgs)))(function () {
                return bind(Spago_Purs.getPurs)(function (purs) {
                    var selected = (function () {
                        if (v.workspace.selected instanceof Data_Maybe.Just) {
                            return [ v.workspace.selected.value0 ];
                        };
                        if (v.workspace.selected instanceof Data_Maybe.Nothing) {
                            return Spago_Config.getWorkspacePackages(v.workspace.packageSet);
                        };
                        throw new Error("Failed pattern match at Main (line 804, column 16 - line 806, column 66): " + [ v.workspace.selected.constructor.name ]);
                    })();
                    return pure({
                        purs: purs,
                        dependencies: dependencies,
                        depsOnly: false,
                        logOptions: v.logOptions,
                        pursArgs: fromFoldable(replArgs.pursArgs),
                        selected: selected
                    });
                });
            });
        });
    };
};
var mkRegistryEnv = /* #__PURE__ */ (function () {
    return discard(logDebug2("CWD: " + Spago_Paths.cwd))(function () {
        return discard(mkdirp(Spago_Paths.globalCachePath))(function () {
            return discard(mkdirp(Spago_Paths.localCachePath))(function () {
                return discard(mkdirp(Spago_Paths.localCachePackagesPath))(function () {
                    return discard(logDebug2("Global cache: " + show4(Spago_Paths.globalCachePath)))(function () {
                        return discard(logDebug2("Local cache: " + show4(Spago_Paths.localCachePath)))(function () {
                            return bind(Spago_Git.getGit)(function (git) {
                                return bind(liftEffect(Effect_Ref["new"](Data_Map_Internal.empty)))(function (indexRef) {
                                    var getManifestFromIndex = function (name) {
                                        return function (version) {
                                            return bind(liftEffect(Effect_Ref.read(indexRef)))(function (indexMap) {
                                                var v = lookup(name)(indexMap);
                                                if (v instanceof Data_Maybe.Just) {
                                                    return pure(lookup1(version)(v.value0));
                                                };
                                                if (v instanceof Data_Maybe.Nothing) {
                                                    return discard(logDebug2("Reading package from Index: " + Registry_PackageName.print(name)))(function () {
                                                        return bind(liftAff(readEntryFile(Spago_Paths.registryIndexPath)(name)))(function (maybeManifests) {
                                                            return bind(map2(map1(function (v1) {
                                                                return new Data_Tuple.Tuple(v1.version, v1);
                                                            }))((function () {
                                                                if (maybeManifests instanceof Data_Either.Right) {
                                                                    return pure(toUnfoldable(maybeManifests.value0));
                                                                };
                                                                if (maybeManifests instanceof Data_Either.Left) {
                                                                    return discard(logWarn("Could not read package manifests from index, proceeding anyways. Error: " + maybeManifests.value0))(function () {
                                                                        return pure([  ]);
                                                                    });
                                                                };
                                                                throw new Error("Failed pattern match at Main (line 863, column 75 - line 867, column 22): " + [ maybeManifests.constructor.name ]);
                                                            })()))(function (manifests) {
                                                                var versions = fromFoldable1(manifests);
                                                                return discard(liftEffect(Effect_Ref.write(insert(name)(versions)(indexMap))(indexRef)))(function () {
                                                                    return pure(lookup1(version)(versions));
                                                                });
                                                            });
                                                        });
                                                    });
                                                };
                                                throw new Error("Failed pattern match at Main (line 857, column 7 - line 870, column 45): " + [ v.constructor.name ]);
                                            });
                                        };
                                    };
                                    return bind(liftEffect(Effect_Ref["new"](Data_Map_Internal.empty)))(function (metadataRef) {
                                        var getMetadata = function (name) {
                                            return bind(liftEffect(Effect_Ref.read(metadataRef)))(function (metadataMap) {
                                                var v = lookup(name)(metadataMap);
                                                if (v instanceof Data_Maybe.Just) {
                                                    return pure(new Data_Either.Right(v.value0));
                                                };
                                                if (v instanceof Data_Maybe.Nothing) {
                                                    var metadataFilePath = Node_Path.concat([ Spago_Paths.registryPath, Registry_Constants.metadataDirectory, Registry_PackageName.print(name) + ".json" ]);
                                                    return discard(logDebug2("Reading metadata from file: " + metadataFilePath))(function () {
                                                        return bind(liftAff(Spago_FS.readJsonFile(Registry_Metadata.codec)(metadataFilePath)))(function (v1) {
                                                            if (v1 instanceof Data_Either.Left) {
                                                                return pure(new Data_Either.Left(v1.value0));
                                                            };
                                                            if (v1 instanceof Data_Either.Right) {
                                                                return discard(liftEffect(Effect_Ref.write(insert(name)(v1.value0)(metadataMap))(metadataRef)))(function () {
                                                                    return pure(new Data_Either.Right(v1.value0));
                                                                });
                                                            };
                                                            throw new Error("Failed pattern match at Main (line 884, column 73 - line 889, column 29): " + [ v1.constructor.name ]);
                                                        });
                                                    });
                                                };
                                                throw new Error("Failed pattern match at Main (line 878, column 7 - line 889, column 29): " + [ v.constructor.name ]);
                                            });
                                        };
                                        return bind(ask)(function (v) {
                                            return discard(whenM(shouldFetchRegistryRepos)(discard(logInfo("Refreshing the Registry Index..."))(function () {
                                                return runSpago({
                                                    logOptions: v.logOptions,
                                                    git: git
                                                })(Spago_Prelude.parallelise([ bind($$try(Spago_Git.fetchRepo({
                                                    git: "https://github.com/purescript/registry-index.git",
                                                    ref: "main"
                                                })(Spago_Paths.registryIndexPath)))(function (v1) {
                                                    if (v1 instanceof Data_Either.Right) {
                                                        return pure(Data_Unit.unit);
                                                    };
                                                    if (v1 instanceof Data_Either.Left) {
                                                        return logWarn("Couldn't refresh the registry-index, will proceed anyways");
                                                    };
                                                    throw new Error("Failed pattern match at Main (line 901, column 130 - line 903, column 91): " + [ v1.constructor.name ]);
                                                }), bind($$try(Spago_Git.fetchRepo({
                                                    git: "https://github.com/purescript/registry.git",
                                                    ref: "main"
                                                })(Spago_Paths.registryPath)))(function (v1) {
                                                    if (v1 instanceof Data_Either.Right) {
                                                        return pure(Data_Unit.unit);
                                                    };
                                                    if (v1 instanceof Data_Either.Left) {
                                                        return logWarn("Couldn't refresh the registry, will proceed anyways");
                                                    };
                                                    throw new Error("Failed pattern match at Main (line 904, column 119 - line 906, column 85): " + [ v1.constructor.name ]);
                                                }) ]));
                                            })))(function () {
                                                return pure({
                                                    getManifestFromIndex: getManifestFromIndex,
                                                    getMetadata: getMetadata,
                                                    logOptions: v.logOptions,
                                                    git: git
                                                });
                                            });
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });
    });
})();
var mkPublishEnv = function (dependencies) {
    return function (purs) {
        return bind(ask)(function (env) {
            return bind((function () {
                if (env.workspace.selected instanceof Data_Maybe.Just) {
                    return pure(env.workspace.selected.value0);
                };
                if (env.workspace.selected instanceof Data_Maybe.Nothing) {
                    var workspacePackages = Spago_Config.getWorkspacePackages(env.workspace.packageSet);
                    if (workspacePackages.length === 1) {
                        return pure(workspacePackages[0]);
                    };
                    return discard(logDebug2(Spago_Prelude.unsafeStringify(workspacePackages)))(function () {
                        return die2([ toDoc("No package was selected for publishing. Please select (with -p) one of the following packages:"), Dodo.indent(toDoc1(map1(function (v) {
                            return v["package"].name;
                        })(workspacePackages))) ]);
                    });
                };
                throw new Error("Failed pattern match at Main (line 779, column 15 - line 793, column 16): " + [ env.workspace.selected.constructor.name ]);
            })())(function (selected) {
                return pure(union({
                    purs: purs,
                    selected: selected,
                    dependencies: dependencies
                })(env));
            });
        });
    };
};
var mkLsEnv = function (dependencies) {
    return bind(ask)(function (v) {
        return bind((function () {
            if (v.workspace.selected instanceof Data_Maybe.Just) {
                return pure(v.workspace.selected.value0);
            };
            if (v.workspace.selected instanceof Data_Maybe.Nothing) {
                var workspacePackages = Spago_Config.getWorkspacePackages(v.workspace.packageSet);
                if (workspacePackages.length === 1) {
                    return pure(workspacePackages[0]);
                };
                return discard(logDebug2(Spago_Prelude.unsafeStringify(workspacePackages)))(function () {
                    return die2([ toDoc("No package was selected. Please select (with -p) one of the following packages:"), Dodo.indent(toDoc1(map1(function (v1) {
                        return v1["package"].name;
                    })(workspacePackages))) ]);
                });
            };
            throw new Error("Failed pattern match at Main (line 919, column 15 - line 933, column 16): " + [ v.workspace.selected.constructor.name ]);
        })())(function (selected) {
            return pure({
                logOptions: v.logOptions,
                workspace: v.workspace,
                dependencies: dependencies,
                selected: selected
            });
        });
    });
};
var mkLogOptions = function (v) {
    return bind2(liftEffect1($foreign.supportsColor))(function (supports) {
        var color = and([ supports, !v.noColor ]);
        var verbosity = (function () {
            if (v.quiet) {
                return Spago_Log.LogQuiet.value;
            };
            if (v.verbose) {
                return Spago_Log.LogVerbose.value;
            };
            return Spago_Log.LogNormal.value;
        })();
        return pure1({
            color: color,
            verbosity: verbosity
        });
    });
};
var mkFetchEnv = function (args) {
    var parsePackageName = function (p) {
        var v = Registry_PackageName.parse(p);
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        if (v instanceof Data_Either.Left) {
            return new Data_Either.Left("- Could not parse package " + (show4(p) + (": " + v.value0)));
        };
        throw new Error("Failed pattern match at Main (line 820, column 26 - line 822, column 79): " + [ v.constructor.name ]);
    };
    var v = partitionMap(parsePackageName)(fromFoldable(args.packages));
    return discard(unless(Data_Array["null"](v.left))(die2(append1([ toDoc("Failed to parse some package name: ") ])(map1(function ($683) {
        return Dodo.indent(toDoc($683));
    })(v.left)))))(function () {
        return bind($$for(args.selectedPackage)(function ($684) {
            return (function (v1) {
                if (v1 instanceof Data_Either.Right) {
                    return pure(v1.value0);
                };
                if (v1 instanceof Data_Either.Left) {
                    return die1("Failed to parse selected package name, was: " + show5(args.selectedPackage));
                };
                throw new Error("Failed pattern match at Main (line 827, column 76 - line 829, column 99): " + [ v1.constructor.name ]);
            })(Registry_PackageName.parse($684));
        }))(function (maybeSelectedPackage) {
            return bind(mkRegistryEnv)(function (env) {
                return bind(runSpago(env)(Spago_Config.readWorkspace(maybeSelectedPackage)))(function (workspace) {
                    var fetchOpts = {
                        packages: v.right,
                        ensureRanges: args.ensureRanges
                    };
                    return pure({
                        fetchOpts: fetchOpts,
                        env: union({
                            workspace: workspace
                        })(env)
                    });
                });
            });
        });
    });
};
var mkBundleEnv = function (bundleArgs) {
    return bind(ask)(function (v) {
        return discard(logDebug2("Bundle args: " + show6(bundleArgs)))(function () {
            return bind((function () {
                if (v.workspace.selected instanceof Data_Maybe.Just) {
                    return pure(v.workspace.selected.value0);
                };
                if (v.workspace.selected instanceof Data_Maybe.Nothing) {
                    var workspacePackageNames = map1(function (v1) {
                        return v1["package"].name;
                    })(Spago_Config.getWorkspacePackages(v.workspace.packageSet));
                    return die2([ toDoc("No package was selected for bundling. Please select (with -p) one of the following packages:"), Dodo.indent(toDoc1(workspacePackageNames)) ]);
                };
                throw new Error("Failed pattern match at Main (line 607, column 15 - line 613, column 155): " + [ v.workspace.selected.constructor.name ]);
            })())(function (selected) {
                return discard(logDebug2("Selected package to bundle: " + Registry_PackageName.print(selected["package"].name)))(function () {
                    var bundleConf = function (f) {
                        return bind1(selected["package"].bundle)(f);
                    };
                    var minify = Data_Array.any(function (v1) {
                        return v1 === true;
                    })([ bundleArgs.minify, Data_Maybe.fromMaybe(false)(bindFlipped(function (v1) {
                        return v1.minify;
                    })(selected["package"].bundle)) ]);
                    var entrypoint = Data_Maybe.fromMaybe("Main")(alt(bundleArgs.module)(bundleConf(function (v1) {
                        return v1.module;
                    })));
                    var outfile = Data_Maybe.fromMaybe("index.js")(alt(bundleArgs.outfile)(bundleConf(function (v1) {
                        return v1.outfile;
                    })));
                    var platform = Data_Maybe.fromMaybe(Spago_Core_Config.BundleBrowser.value)(alt(bindFlipped(Spago_Core_Config.parsePlatform)(bundleArgs.platform))(bundleConf(function (v1) {
                        return v1.platform;
                    })));
                    var bundleType = Data_Maybe.fromMaybe(Spago_Core_Config.BundleApp.value)(alt(bindFlipped(Spago_Core_Config.parseBundleType)(bundleArgs.type))(bundleConf(function (v1) {
                        return v1.type;
                    })));
                    var bundleOptions = {
                        minify: minify,
                        module: entrypoint,
                        outfile: outfile,
                        platform: platform,
                        type: bundleType
                    };
                    var newWorkspace = {
                        buildOptions: {
                            output: alt(bundleArgs.output)(v.workspace.buildOptions.output),
                            pedanticPackages: bundleArgs.pedanticPackages || v.workspace.buildOptions.pedanticPackages,
                            censorBuildWarnings: v.workspace.buildOptions.censorBuildWarnings,
                            censorCodes: v.workspace.buildOptions.censorCodes,
                            filterCodes: v.workspace.buildOptions.filterCodes,
                            persistWarnings: v.workspace.buildOptions.persistWarnings,
                            showSource: v.workspace.buildOptions.showSource,
                            statVerbosity: v.workspace.buildOptions.statVerbosity,
                            strict: v.workspace.buildOptions.strict
                        },
                        packageSet: v.workspace.packageSet,
                        selected: v.workspace.selected,
                        backend: v.workspace.backend,
                        compatibleCompiler: v.workspace.compatibleCompiler,
                        doc: v.workspace.doc,
                        lockfile: v.workspace.lockfile,
                        originalConfig: v.workspace.originalConfig
                    };
                    return bind(Spago_Esbuild.getEsbuild)(function (esbuild) {
                        var bundleEnv = {
                            esbuild: esbuild,
                            logOptions: v.logOptions,
                            workspace: newWorkspace,
                            selected: selected,
                            bundleOptions: bundleOptions
                        };
                        return pure(bundleEnv);
                    });
                });
            });
        });
    });
};
var mkBuildEnv = function (buildArgs) {
    return function (dependencies) {
        return bind(ask)(function (v) {
            return bind(Spago_Purs.getPurs)(function (purs) {
                var newWorkspace = {
                    buildOptions: {
                        output: alt(buildArgs.output)(v.workspace.buildOptions.output),
                        pedanticPackages: buildArgs.pedanticPackages || v.workspace.buildOptions.pedanticPackages,
                        censorBuildWarnings: v.workspace.buildOptions.censorBuildWarnings,
                        censorCodes: v.workspace.buildOptions.censorCodes,
                        filterCodes: v.workspace.buildOptions.filterCodes,
                        persistWarnings: v.workspace.buildOptions.persistWarnings,
                        showSource: v.workspace.buildOptions.showSource,
                        statVerbosity: v.workspace.buildOptions.statVerbosity,
                        strict: v.workspace.buildOptions.strict
                    },
                    backend: map3(function (b) {
                        var v2 = Data_List["null"](buildArgs.backendArgs);
                        if (v2) {
                            return b;
                        };
                        if (!v2) {
                            return {
                                args: new Data_Maybe.Just(fromFoldable(buildArgs.backendArgs)),
                                cmd: b.cmd
                            };
                        };
                        throw new Error("Failed pattern match at Main (line 761, column 19 - line 763, column 84): " + [ v2.constructor.name ]);
                    })(v.workspace.backend),
                    compatibleCompiler: v.workspace.compatibleCompiler,
                    doc: v.workspace.doc,
                    lockfile: v.workspace.lockfile,
                    originalConfig: v.workspace.originalConfig,
                    packageSet: v.workspace.packageSet,
                    selected: v.workspace.selected
                };
                return pure({
                    logOptions: v.logOptions,
                    purs: purs,
                    git: v.git,
                    dependencies: dependencies,
                    workspace: newWorkspace
                });
            });
        });
    };
};
var lsPackagesArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons14(Optparse.buildRecordArgsNil))({
    json: Spago_Bin_Flags.json
});
var lsDepsArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons14(/* #__PURE__ */ buildArgsCons9(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "transitive";
    }
})()()()(Optparse.buildRecordArgsNil))))({
    json: Spago_Bin_Flags.json,
    transitive: Spago_Bin_Flags.transitive,
    selectedPackage: Spago_Bin_Flags.selectedPackage
});
var installArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons(/* #__PURE__ */ buildArgsCons16(/* #__PURE__ */ buildArgsCons5(/* #__PURE__ */ buildArgsCons19(/* #__PURE__ */ buildArgsCons6(buildArgsCons17))))))({
    packages: Spago_Bin_Flags.packages,
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    pursArgs: Spago_Bin_Flags.pursArgs,
    backendArgs: Spago_Bin_Flags.backendArgs,
    output: Spago_Bin_Flags.output,
    pedanticPackages: Spago_Bin_Flags.pedanticPackages,
    ensureRanges: Spago_Bin_Flags.ensureRanges
});
var initArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "name";
    }
})()()()(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "setVersion";
    }
})()()()(Optparse.buildRecordArgsNil)))({
    setVersion: Spago_Bin_Flags.maybeSetVersion,
    name: Spago_Bin_Flags.maybePackageName
});

/**
 * 
 * 
 * TODO: add flag for overriding the cache location
 * 
 *     buildOptions  = BuildOptions <$> watch <*> clearScreen <*> allowIgnored <*> sourcePaths <*> srcMapFlag <*> noInstall
 *                     <*> pursArgs <*> depsOnly <*> beforeCommands <*> thenCommands <*> elseCommands
 * 
 */
// https://stackoverflow.com/questions/45395369/how-to-get-console-log-line-numbers-shown-in-nodejs
// TODO: veryVerbose = CLI.switch "very-verbose" 'V' "Enable more verbosity: timestamps and source locations"
var globalArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "noColor";
    }
})()()()(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "quiet";
    }
})()()()(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "verbose";
    }
})()()()(Optparse.buildRecordArgsNil))))({
    quiet: Spago_Bin_Flags.quiet,
    verbose: Spago_Bin_Flags.verbose,
    noColor: Spago_Bin_Flags.noColor
});
var fetchArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons16(/* #__PURE__ */ buildArgsCons19(buildArgsCons15)))({
    packages: Spago_Bin_Flags.packages,
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    ensureRanges: Spago_Bin_Flags.ensureRanges
});
var commandParser = function (command_) {
    return function (parser_) {
        return function (description_) {
            return Options_Applicative_Builder.command(command_)(Options_Applicative_Builder.info(apply(map4(SpagoCmd.create)(globalArgsParser))(parser_))(Options_Applicative_Builder.progDesc(description_)));
        };
    };
};
var bundleArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons(/* #__PURE__ */ buildArgsCons1(/* #__PURE__ */ buildArgsCons2(/* #__PURE__ */ buildArgsCons16(/* #__PURE__ */ buildArgsCons4(/* #__PURE__ */ Optparse.buildArgsCons(minifyIsSymbol)()()()(/* #__PURE__ */ Optparse.buildArgsCons(moduleIsSymbol)()()()(/* #__PURE__ */ Optparse.buildArgsCons(outfileIsSymbol)()()()(/* #__PURE__ */ buildArgsCons5(/* #__PURE__ */ buildArgsCons6(/* #__PURE__ */ buildArgsCons7(/* #__PURE__ */ Optparse.buildArgsCons(platformIsSymbol)()()()(/* #__PURE__ */ buildArgsCons8(/* #__PURE__ */ buildArgsCons9(/* #__PURE__ */ buildArgsCons10(/* #__PURE__ */ buildArgsCons11(/* #__PURE__ */ buildArgsCons12(/* #__PURE__ */ Optparse.buildArgsCons(typeIsSymbol)()()()(Optparse.buildRecordArgsNil)))))))))))))))))))({
    minify: Spago_Bin_Flags.minify,
    module: Spago_Bin_Flags.entrypoint,
    type: Spago_Bin_Flags.bundleType,
    outfile: Spago_Bin_Flags.outfile,
    platform: Spago_Bin_Flags.platform,
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    pursArgs: Spago_Bin_Flags.pursArgs,
    backendArgs: Spago_Bin_Flags.backendArgs,
    output: Spago_Bin_Flags.output,
    pedanticPackages: Spago_Bin_Flags.pedanticPackages,
    ensureRanges: Spago_Bin_Flags.ensureRanges,
    strict: Spago_Bin_Flags.strict,
    censorBuildWarnings: Spago_Bin_Flags.censorBuildWarnings,
    showSource: Spago_Bin_Flags.showSource,
    censorCodes: Spago_Bin_Flags.censorCodes,
    filterCodes: Spago_Bin_Flags.filterCodes,
    statVerbosity: Spago_Bin_Flags.statVerbosity,
    persistWarnings: Spago_Bin_Flags.persistWarnings
});
var buildArgsParser = /* #__PURE__ */ fromRecord(/* #__PURE__ */ buildArgsCons(/* #__PURE__ */ buildArgsCons1(/* #__PURE__ */ buildArgsCons2(/* #__PURE__ */ buildArgsCons16(/* #__PURE__ */ buildArgsCons4(/* #__PURE__ */ Optparse.buildArgsCons({
    reflectSymbol: function () {
        return "jsonErrors";
    }
})()()()(buildArgsCons13)))))))({
    selectedPackage: Spago_Bin_Flags.selectedPackage,
    pursArgs: Spago_Bin_Flags.pursArgs,
    backendArgs: Spago_Bin_Flags.backendArgs,
    output: Spago_Bin_Flags.output,
    pedanticPackages: Spago_Bin_Flags.pedanticPackages,
    ensureRanges: Spago_Bin_Flags.ensureRanges,
    jsonErrors: Spago_Bin_Flags.jsonErrors,
    strict: Spago_Bin_Flags.strict,
    censorBuildWarnings: Spago_Bin_Flags.censorBuildWarnings,
    showSource: Spago_Bin_Flags.showSource,
    censorCodes: Spago_Bin_Flags.censorCodes,
    filterCodes: Spago_Bin_Flags.filterCodes,
    statVerbosity: Spago_Bin_Flags.statVerbosity,
    persistWarnings: Spago_Bin_Flags.persistWarnings
});
var argParser = /* #__PURE__ */ (function () {
    return Options_Applicative_Extra.hsubparser(fold([ commandParser("init")(map4(Init.create)(initArgsParser))("Initialise a new project"), commandParser("fetch")(map4(Fetch.create)(fetchArgsParser))("Downloads all of the project's dependencies"), commandParser("install")(map4(Install.create)(installArgsParser))("Compile the project's dependencies"), commandParser("build")(map4(Build.create)(buildArgsParser))("Compile the project"), commandParser("run")(map4(Run.create)(runArgsParser))("Run the project"), commandParser("test")(map4(Test.create)(testArgsParser))("Test the project"), commandParser("bundle")(map4(Bundle.create)(bundleArgsParser))("Bundle the project in a single file"), commandParser("sources")(map4(Sources.create)(sourcesArgsParser))("List all the source paths (globs) for the dependencies of the project"), commandParser("repl")(map4(Repl.create)(replArgsParser))("Start a REPL"), commandParser("publish")(map4(Publish.create)(publishArgsParser))("Publish a package"), Options_Applicative_Builder.command("registry")(Options_Applicative_Builder.info(Options_Applicative_Extra.hsubparser(fold([ commandParser("search")(map4(RegistrySearch.create)(registrySearchArgsParser))("Search for package names in the Registry"), commandParser("info")(map4(RegistryInfo.create)(registryInfoArgsParser))("Query the Registry for information about packages and versions") ])))(Options_Applicative_Builder.progDesc("Commands to interact with the Registry"))), Options_Applicative_Builder.command("ls")(Options_Applicative_Builder.info(Options_Applicative_Extra.hsubparser(fold([ commandParser("packages")(map4(LsPackages.create)(lsPackagesArgsParser))("List packages available in the local package set"), commandParser("deps")(map4(LsDeps.create)(lsDepsArgsParser))("List dependencies of the project") ])))(Options_Applicative_Builder.progDesc("List packages or dependencies"))) ]));
})();
var parseArgs = /* #__PURE__ */ (function () {
    return Options_Applicative_Extra.customExecParser((function (v) {
        return {
            prefMultiSuffix: v.prefMultiSuffix,
            prefDisambiguate: v.prefDisambiguate,
            prefShowHelpOnError: true,
            prefShowHelpOnEmpty: true,
            prefBacktrack: v.prefBacktrack,
            prefColumns: v.prefColumns
        };
    })(Options_Applicative_Builder.defaultPrefs))(Options_Applicative_Builder.info(apply(Options_Applicative_Extra.helper)(Control_Alt.alt(Options_Applicative_Types.parserAlt)(map4(Data_Either.Left.create)(argParser))(map4(Data_Either.Right.create)(Options_Applicative_Builder["switch"](append2(Options_Applicative_Builder["long"](Options_Applicative_Builder_Internal.flagFieldsHasName)("version"))(append2(Options_Applicative_Builder["short"](Options_Applicative_Builder_Internal.flagFieldsHasName)("v"))(Options_Applicative_Builder.help("Show the current version"))))))))(Options_Applicative_Builder.progDesc("PureScript package manager and build tool")));
})();
var main = /* #__PURE__ */ (function () {
    var printVersion = bind2(mkLogOptions({
        noColor: false,
        quiet: false,
        verbose: false
    }))(function (logOptions) {
        return runSpago1({
            logOptions: logOptions
        })(logInfo(Spago_Generated_BuildInfo.buildInfo.spagoVersion));
    });
    return function __do() {
        var c = parseArgs();
        return Effect_Aff.launchAff_((function () {
            if (c instanceof Data_Either.Left) {
                return bind2(mkLogOptions(c.value0.value0))(function (logOptions) {
                    return runSpago1({
                        logOptions: logOptions
                    })((function () {
                        if (c.value0.value1 instanceof Sources) {
                            return bind(mkFetchEnv({
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: false
                            }))(function (v) {
                                return $$void(runSpago(v.env)(Spago_Command_Sources.run({
                                    json: c.value0.value1.value0.json
                                })));
                            });
                        };
                        if (c.value0.value1 instanceof Init) {
                            return bind(Spago_Purs.getPurs)(function (purs) {
                                var candidateName = Data_Maybe.fromMaybe(Data_String_CodePoints.take(50)(Node_Path.basename(Spago_Paths.cwd)))(c.value0.value1.value0.name);
                                return discard(logDebug1([ show4(Spago_Paths.cwd), show4(candidateName) ]))(function () {
                                    return bind((function () {
                                        var v = Registry_PackageName.parse(Registry_PackageName.stripPureScriptPrefix(candidateName));
                                        if (v instanceof Data_Either.Left) {
                                            return die3([ "Could not figure out a name for the new package. Error:", show4(v.value0) ]);
                                        };
                                        if (v instanceof Data_Either.Right) {
                                            return pure(v.value0);
                                        };
                                        throw new Error("Failed pattern match at Main (line 441, column 28 - line 443, column 32): " + [ v.constructor.name ]);
                                    })())(function (packageName) {
                                        return bind($$for(c.value0.value1.value0.setVersion)(function ($685) {
                                            return (function (v) {
                                                if (v instanceof Data_Either.Left) {
                                                    return die3([ "Could not parse provided set version. Error:", show4(v.value0) ]);
                                                };
                                                if (v instanceof Data_Either.Right) {
                                                    return pure(v.value0);
                                                };
                                                throw new Error("Failed pattern match at Main (line 444, column 73 - line 446, column 32): " + [ v.constructor.name ]);
                                            })(Spago_Prelude.parseLenientVersion($685));
                                        }))(function (setVersion) {
                                            return discard(logDebug1([ "Got packageName and setVersion:", Registry_PackageName.print(packageName), Spago_Prelude.unsafeStringify(setVersion) ]))(function () {
                                                var initOpts = {
                                                    packageName: packageName,
                                                    setVersion: setVersion
                                                };
                                                return discard($$void(mkRegistryEnv))(function () {
                                                    return discard($$void(runSpago({
                                                        logOptions: logOptions,
                                                        purs: purs
                                                    })(Spago_Command_Init.run(initOpts))))(function () {
                                                        return discard(logInfo("Set up a new Spago project."))(function () {
                                                            return logInfo("Try running `spago run`");
                                                        });
                                                    });
                                                });
                                            });
                                        });
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Fetch) {
                            return bind(mkFetchEnv(c.value0.value1.value0))(function (v) {
                                return $$void(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)));
                            });
                        };
                        if (c.value0.value1 instanceof RegistrySearch) {
                            return bind(mkRegistryEnv)(function (env) {
                                return $$void(runSpago(env)(Spago_Command_Registry.search(c["value0"]["value1"]["value0"]["package"])));
                            });
                        };
                        if (c.value0.value1 instanceof RegistryInfo) {
                            return bind(mkRegistryEnv)(function (env) {
                                return $$void(runSpago(env)(Spago_Command_Registry.info(c.value0.value1.value0)));
                            });
                        };
                        if (c.value0.value1 instanceof Install) {
                            return bind(mkFetchEnv({
                                packages: c.value0.value1.value0.packages,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: c.value0.value1.value0.ensureRanges
                            }))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    var buildArgs = merge(c.value0.value1.value0)({
                                        censorBuildWarnings: Data_Maybe.Nothing.value,
                                        censorCodes: Data_Maybe.Nothing.value,
                                        filterCodes: Data_Maybe.Nothing.value,
                                        statVerbosity: Data_Maybe.Nothing.value,
                                        showSource: Data_Maybe.Nothing.value,
                                        strict: Data_Maybe.Nothing.value,
                                        persistWarnings: Data_Maybe.Nothing.value
                                    });
                                    return bind(runSpago(v.env)(mkBuildEnv(buildArgs)(dependencies)))(function (env$prime) {
                                        var options = {
                                            depsOnly: true,
                                            pursArgs: toUnfoldable1(c.value0.value1.value0.pursArgs),
                                            jsonErrors: false
                                        };
                                        return runSpago(env$prime)(Spago_Command_Build.run(options));
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Build) {
                            return bind(mkFetchEnv({
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: c.value0.value1.value0.ensureRanges
                            }))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    return bind(runSpago(v.env)(mkBuildEnv(c.value0.value1.value0)(dependencies)))(function (buildEnv) {
                                        var options = {
                                            depsOnly: false,
                                            pursArgs: toUnfoldable1(c.value0.value1.value0.pursArgs),
                                            jsonErrors: c.value0.value1.value0.jsonErrors
                                        };
                                        return runSpago(buildEnv)(Spago_Command_Build.run(options));
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Publish) {
                            return bind(mkFetchEnv({
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: false
                            }))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    var buildArgs = {
                                        selectedPackage: c.value0.value1.value0.selectedPackage,
                                        backendArgs: mempty,
                                        output: mempty1,
                                        pedanticPackages: false,
                                        ensureRanges: false,
                                        jsonErrors: false,
                                        censorBuildWarnings: Data_Maybe.Nothing.value,
                                        censorCodes: Data_Maybe.Nothing.value,
                                        filterCodes: Data_Maybe.Nothing.value,
                                        statVerbosity: Data_Maybe.Nothing.value,
                                        showSource: Data_Maybe.Nothing.value,
                                        strict: Data_Maybe.Nothing.value,
                                        persistWarnings: Data_Maybe.Nothing.value
                                    };
                                    return bind(runSpago(v.env)(mkBuildEnv(buildArgs)(dependencies)))(function (v1) {
                                        return bind(runSpago(v.env)(mkPublishEnv(dependencies)(v1.purs)))(function (publishEnv) {
                                            return $$void(runSpago(publishEnv)(Spago_Command_Publish.publish({})));
                                        });
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Repl) {
                            return bind(bind(exists("spago.yaml"))(function (v) {
                                if (v) {
                                    return pure(mempty);
                                };
                                if (!v) {
                                    return discard(logWarn("No configuration found, creating a temporary project to run a repl in..."))(function () {
                                        return bind(mkTemp)(function (tmpDir) {
                                            return discard(mkdirp(tmpDir))(function () {
                                                return discard(logDebug2("Creating repl project in temp dir: " + tmpDir))(function () {
                                                    return discard(liftEffect(Node_Process.chdir(tmpDir)))(function () {
                                                        return bind(Spago_Purs.getPurs)(function (purs) {
                                                            return discard($$void(runSpago({
                                                                purs: purs,
                                                                logOptions: logOptions
                                                            })(Spago_Command_Init.run({
                                                                setVersion: Data_Maybe.Nothing.value,
                                                                packageName: "repl"
                                                            }))))(function () {
                                                                return pure(fromFoldable2([ "effect", "console" ]));
                                                            });
                                                        });
                                                    });
                                                });
                                            });
                                        });
                                    });
                                };
                                throw new Error("Failed pattern match at Main (line 514, column 52 - line 527, column 65): " + [ v.constructor.name ]);
                            }))(function (packages) {
                                return bind(mkFetchEnv({
                                    packages: packages,
                                    selectedPackage: c.value0.value1.value0.selectedPackage,
                                    ensureRanges: false
                                }))(function (v) {
                                    return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                        return bind(runSpago(v.env)(Spago_Command_Repl.supportPackage(v.env.workspace.packageSet)))(function (supportPackages) {
                                            return bind(runSpago(v.env)(mkReplEnv(c.value0.value1.value0)(union1(dependencies)(supportPackages))))(function (replEnv) {
                                                return $$void(runSpago(replEnv)(Spago_Command_Repl.run));
                                            });
                                        });
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Bundle) {
                            return bind(mkFetchEnv({
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: c.value0.value1.value0.ensureRanges
                            }))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    return bind(runSpago(v.env)(mkBuildEnv(c.value0.value1.value0)(dependencies)))(function (buildEnv) {
                                        var options = {
                                            depsOnly: false,
                                            pursArgs: toUnfoldable1(c.value0.value1.value0.pursArgs),
                                            jsonErrors: false
                                        };
                                        return discard(runSpago(buildEnv)(Spago_Command_Build.run(options)))(function () {
                                            return bind(runSpago(v.env)(mkBundleEnv(c.value0.value1.value0)))(function (bundleEnv) {
                                                return runSpago(bundleEnv)(Spago_Command_Bundle.run);
                                            });
                                        });
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Run) {
                            return bind(mkFetchEnv({
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: c.value0.value1.value0.ensureRanges
                            }))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    return bind(runSpago(v.env)(mkBuildEnv(c.value0.value1.value0)(dependencies)))(function (buildEnv) {
                                        var options = {
                                            depsOnly: false,
                                            pursArgs: toUnfoldable1(c.value0.value1.value0.pursArgs),
                                            jsonErrors: false
                                        };
                                        return discard(runSpago(buildEnv)(Spago_Command_Build.run(options)))(function () {
                                            return bind(runSpago(v.env)(mkRunEnv(c.value0.value1.value0)(buildEnv)))(function (runEnv) {
                                                return runSpago(runEnv)(Spago_Command_Run.run);
                                            });
                                        });
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof Test) {
                            return bind(mkFetchEnv({
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: false
                            }))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    return bind(runSpago(v.env)(mkBuildEnv(union(c.value0.value1.value0)({
                                        ensureRanges: false
                                    }))(dependencies)))(function (buildEnv) {
                                        var options = {
                                            depsOnly: false,
                                            pursArgs: toUnfoldable1(c.value0.value1.value0.pursArgs),
                                            jsonErrors: false
                                        };
                                        return discard(runSpago(buildEnv)(Spago_Command_Build.run(options)))(function () {
                                            return bind(runSpago(v.env)(mkTestEnv(c.value0.value1.value0)(buildEnv)))(function (testEnv) {
                                                return runSpago(testEnv)(Spago_Command_Test.run);
                                            });
                                        });
                                    });
                                });
                            });
                        };
                        if (c.value0.value1 instanceof LsPackages) {
                            var fetchArgs = {
                                packages: mempty,
                                selectedPackage: Data_Maybe.Nothing.value,
                                ensureRanges: false
                            };
                            return bind(mkFetchEnv(fetchArgs))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    var lsEnv = {
                                        workspace: v.env.workspace,
                                        dependencies: dependencies,
                                        logOptions: logOptions
                                    };
                                    return runSpago(lsEnv)(Spago_Command_Ls.listPackageSet(c.value0.value1.value0));
                                });
                            });
                        };
                        if (c.value0.value1 instanceof LsDeps) {
                            var fetchArgs = {
                                packages: mempty,
                                selectedPackage: c.value0.value1.value0.selectedPackage,
                                ensureRanges: false
                            };
                            return bind(mkFetchEnv(fetchArgs))(function (v) {
                                return bind(runSpago(v.env)(Spago_Command_Fetch.run(v.fetchOpts)))(function (dependencies) {
                                    return bind(runSpago(v.env)(mkLsEnv(dependencies)))(function (lsEnv) {
                                        return runSpago(lsEnv)(Spago_Command_Ls.listPackages({
                                            json: c.value0.value1.value0.json,
                                            transitive: c.value0.value1.value0.transitive
                                        }));
                                    });
                                });
                            });
                        };
                        throw new Error("Failed pattern match at Main (line 432, column 33 - line 581, column 66): " + [ c.value0.value1.constructor.name ]);
                    })());
                });
            };
            if (c instanceof Data_Either.Right) {
                return when(c.value0)(printVersion);
            };
            throw new Error("Failed pattern match at Main (line 429, column 26 - line 582, column 40): " + [ c.constructor.name ]);
        })())();
    };
})();
export {
    main
};
