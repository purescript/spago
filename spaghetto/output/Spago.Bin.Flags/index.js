import * as Control_Alt from "../Control.Alt/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Set_NonEmpty from "../Data.Set.NonEmpty/index.js";
import * as Options_Applicative_Builder from "../Options.Applicative.Builder/index.js";
import * as Options_Applicative_Builder_Internal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as Spago_Core_Config from "../Spago.Core.Config/index.js";
var append = /* #__PURE__ */ Data_Semigroup.append(Options_Applicative_Builder_Internal.modSemigroup);
var $$long = /* #__PURE__ */ Options_Applicative_Builder["long"](Options_Applicative_Builder_Internal.flagFieldsHasName);
var $$short = /* #__PURE__ */ Options_Applicative_Builder["short"](Options_Applicative_Builder_Internal.flagFieldsHasName);
var optional = /* #__PURE__ */ Options_Applicative_Types.optional(Options_Applicative_Types.parserAlt)(Options_Applicative_Types.parserApplicative);
var voidRight = /* #__PURE__ */ Data_Functor.voidRight(Options_Applicative_Types.parserFunctor);
var long1 = /* #__PURE__ */ Options_Applicative_Builder["long"](Options_Applicative_Builder_Internal.optionFieldsHasName);
var short1 = /* #__PURE__ */ Options_Applicative_Builder["short"](Options_Applicative_Builder_Internal.optionFieldsHasName);
var map = /* #__PURE__ */ Data_Functor.map(Options_Applicative_Types.parserFunctor);
var metavar = /* #__PURE__ */ Options_Applicative_Builder.metavar(Options_Applicative_Builder_Internal.optionFieldsHasMetavar);
var fromFoldable = /* #__PURE__ */ Data_Set_NonEmpty.fromFoldable(Data_List_Types.foldableList)(Data_Ord.ordString);
var verbose = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("verbose"))(/* #__PURE__ */ append(/* #__PURE__ */ $$short("v"))(/* #__PURE__ */ Options_Applicative_Builder.help("Enable additional debug logging, e.g. printing `purs` commands"))));
var transitive = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("transitive"))(/* #__PURE__ */ Options_Applicative_Builder.help("Include transitive dependencies")));
var testDeps = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("test-deps"))(/* #__PURE__ */ Options_Applicative_Builder.help("Act on the test config rather than the main one")));
var strict = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("strict"))(/* #__PURE__ */ Options_Applicative_Builder.help("Promotes project sources' warnings to errors"))));
var statVerbosity = /* #__PURE__ */ (function () {
    return optional(Control_Alt.alt(Options_Applicative_Types.parserAlt)(voidRight(Spago_Core_Config.VerboseStats.value)(Options_Applicative_Builder["switch"](append($$long("verbose-stats"))(Options_Applicative_Builder.help("Show counts for each warning type")))))(voidRight(Spago_Core_Config.NoStats.value)(Options_Applicative_Builder["switch"](append($$long("censor-stats"))(Options_Applicative_Builder.help("Censor warning/error summary"))))));
})();
var showSource = /* #__PURE__ */ (function () {
    return optional(Data_Functor.voidLeft(Options_Applicative_Types.parserFunctor)(Options_Applicative_Builder["switch"](append($$long("no-source"))(Options_Applicative_Builder.help("Disable original source code printing"))))(Spago_Core_Config.NoSourceCode.value));
})();
var selectedPackage = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("package"))(/* #__PURE__ */ append(/* #__PURE__ */ short1("p"))(/* #__PURE__ */ Options_Applicative_Builder.help("Select the local project to build")))));
var quiet = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("quiet"))(/* #__PURE__ */ append(/* #__PURE__ */ $$short("q"))(/* #__PURE__ */ Options_Applicative_Builder.help("Suppress all spago logging"))));
var pursArgs = /* #__PURE__ */ map(/* #__PURE__ */ Data_List.fromFoldable(Data_List_Types.foldableList))(/* #__PURE__ */ Options_Applicative_Types.many(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("purs-args"))(/* #__PURE__ */ append(/* #__PURE__ */ metavar("ARGS"))(/* #__PURE__ */ Options_Applicative_Builder.help("Arguments to pass to purs compile. Wrap in quotes. `--output` and `--json-errors` must be passed to Spago directly."))))));

// TODO make an ADT for node and browser
var platform = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.option(/* #__PURE__ */ Options_Applicative_Builder.eitherReader(function (v) {
    if (v === "node") {
        return new Data_Either.Right("node");
    };
    if (v === "browser") {
        return new Data_Either.Right("browser");
    };
    return new Data_Either.Left("Expected \"node\" or \"browser\"");
}))(/* #__PURE__ */ append(/* #__PURE__ */ long1("platform"))(/* #__PURE__ */ Options_Applicative_Builder.help("The bundle platform. 'node' or 'browser'"))));
var persistWarnings = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("persist-warnings"))(/* #__PURE__ */ Options_Applicative_Builder.help("Persist the compiler warnings between multiple underlying `purs compile` calls"))));
var pedanticPackages = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("pedantic-packages"))(/* #__PURE__ */ Options_Applicative_Builder.help("Check for redundant or missing packages in the config and fail the build if any")));
var packages = /* #__PURE__ */ Options_Applicative_Types.many(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ metavar("PACKAGE"))(/* #__PURE__ */ Options_Applicative_Builder.help("Package name to add as dependency"))));
var $$package = /* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ metavar("PACKAGE"))(/* #__PURE__ */ Options_Applicative_Builder.help("Package name")));
var output = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("output"))(/* #__PURE__ */ append(/* #__PURE__ */ Options_Applicative_Builder.help("The output directory for compiled files"))(/* #__PURE__ */ append(/* #__PURE__ */ metavar("DIR"))(/* #__PURE__ */ Options_Applicative_Builder.value(Options_Applicative_Builder_Internal.optionFieldsHasValue)("output"))))));
var outfile = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("outfile"))(/* #__PURE__ */ Options_Applicative_Builder.help("Destination path for the bundle"))));
var noColor = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("no-color"))(/* #__PURE__ */ append(/* #__PURE__ */ $$long("monochrome"))(/* #__PURE__ */ Options_Applicative_Builder.help("Force logging without ANSI color escape sequences"))));
var moduleName = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("main"))(/* #__PURE__ */ append(/* #__PURE__ */ short1("m"))(/* #__PURE__ */ Options_Applicative_Builder.help("Module to be used as the application's entry point")))));
var minify = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("minify"))(/* #__PURE__ */ Options_Applicative_Builder.help("Minify the bundle")));
var maybeVersion = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ metavar("VERSION"))(/* #__PURE__ */ Options_Applicative_Builder.help("Package version"))));
var maybeSetVersion = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("package-set"))(/* #__PURE__ */ Options_Applicative_Builder.help("Optional package set version to be used instead of the latest one"))));
var maybePackageName = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("name"))(/* #__PURE__ */ Options_Applicative_Builder.help("Optional package name to be used for the new project"))));
var jsonErrors = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("json-errors"))(/* #__PURE__ */ Options_Applicative_Builder.help("Output compiler warnings/errors as JSON")));
var json = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("json"))(/* #__PURE__ */ Options_Applicative_Builder.help("Format the output as JSON")));
var filterCodes = /* #__PURE__ */ map(fromFoldable)(/* #__PURE__ */ Options_Applicative_Types.many(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("filter-code"))(/* #__PURE__ */ append(/* #__PURE__ */ metavar("CODE"))(/* #__PURE__ */ Options_Applicative_Builder.help("Only show a specific error code (e.g. `TypesDoNotUnify`)"))))));
var execArgs = /* #__PURE__ */ optional(/* #__PURE__ */ map(/* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList))(/* #__PURE__ */ Options_Applicative_Types.many(/* #__PURE__ */ Options_Applicative_Builder.strArgument(/* #__PURE__ */ append(/* #__PURE__ */ Options_Applicative_Builder.help("Arguments to pass to the running script"))(/* #__PURE__ */ Options_Applicative_Builder.metavar(Options_Applicative_Builder_Internal.argumentFieldsHasMetavar)("ARGS"))))));
var entrypoint = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("module"))(/* #__PURE__ */ Options_Applicative_Builder.help("The module to bundle as the entrypoint"))));
var ensureRanges = /* #__PURE__ */ Options_Applicative_Builder["switch"](/* #__PURE__ */ append(/* #__PURE__ */ $$long("ensure-ranges"))(/* #__PURE__ */ Options_Applicative_Builder.help("Add version bounds for all the dependencies of the selected project")));
var censorCodes = /* #__PURE__ */ map(fromFoldable)(/* #__PURE__ */ Options_Applicative_Types.many(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("censor-code"))(/* #__PURE__ */ append(/* #__PURE__ */ metavar("CODE"))(/* #__PURE__ */ Options_Applicative_Builder.help("Censor a specific error code (e.g. `ShadowedName`)"))))));
var censorBuildWarnings = /* #__PURE__ */ optional(/* #__PURE__ */ Data_Function.flip(Options_Applicative_Builder.option)(/* #__PURE__ */ append(/* #__PURE__ */ long1("censor-build-warnings"))(/* #__PURE__ */ append(/* #__PURE__ */ Options_Applicative_Builder.help("Censor compiler warnings based on file's location: 'dependency', 'project', or 'all'"))(/* #__PURE__ */ metavar("ARG"))))(/* #__PURE__ */ Options_Applicative_Builder.eitherReader(function (v) {
    if (v === "all") {
        return new Data_Either.Right(Spago_Core_Config.CensorAllWarnings.value);
    };
    if (v === "project") {
        return new Data_Either.Right(Spago_Core_Config.CensorProjectWarnings.value);
    };
    if (v === "dependency") {
        return new Data_Either.Right(Spago_Core_Config.CensorDependencyWarnings.value);
    };
    return new Data_Either.Left("Expected 'all', 'project', or 'dependency'");
})));
var bundleType = /* #__PURE__ */ optional(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("type"))(/* #__PURE__ */ Options_Applicative_Builder.help("The type of the module produced. 'app' will call main, 'module' will just export the contents."))));
var backendArgs = /* #__PURE__ */ Options_Applicative_Types.many(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ append(/* #__PURE__ */ long1("backend-args"))(/* #__PURE__ */ append(/* #__PURE__ */ Options_Applicative_Builder.help("Arguments to pass to the running script"))(/* #__PURE__ */ metavar("ARGS")))));
export {
    selectedPackage,
    strict,
    censorBuildWarnings,
    showSource,
    censorCodes,
    filterCodes,
    statVerbosity,
    persistWarnings,
    jsonErrors,
    minify,
    entrypoint,
    bundleType,
    outfile,
    platform,
    output,
    quiet,
    verbose,
    noColor,
    json,
    transitive,
    pedanticPackages,
    pursArgs,
    execArgs,
    backendArgs,
    moduleName,
    testDeps,
    packages,
    $$package as package,
    maybeVersion,
    maybeSetVersion,
    maybePackageName,
    ensureRanges
};
