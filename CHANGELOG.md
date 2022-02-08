# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.20.5] - 2022-02-08

Features:
- Drop need for `psci-support` to be defined in `dependencies` field (#817)

Other improvements:
- Upgrade to latest Dhall version (#848, #851)

## [0.20.4] - 2022-01-31

Bugfixes:
- Don't warn on unused deps when building with `--deps-only` (#794)
- Add line buffering for console output when doing parallel work (#800, #729)
- Avoid writing a JS file when executing `spago run` (#845, #846, #822)

Other improvements:
- CI: Add sha256 checksum generation on the release workflow (#816)
- CI: Update the Ubuntu runner to a non-deprecated version (#826)
- Install: replace deprecated `request` by `make-fetch-happen` for NPM installation (#840)
- Error messages: output additional info when failing to access cache directory (#799)
- Deps: add compatibility with versions-5.0.0 (#798)
- Internals: convert deps and source paths from List to Set (#818)
- Docs: various improvements (#802, #803, #806, #809, #823, #824)

## [0.20.3] - 2021-05-12

Bugfixes:
- Fix `docs` command error due to bad templates (#792)

## [0.20.2] - 2021-05-06

Bugfixes:
- Remove `npm install` from release CI to prevent overwriting the install script with the Linux binary (#783, #786)
- Use spago.cabal instead of package.yaml to get version number (#787, #788)
- Assume compatibility with newer minor versions of `purs` (#782, #777)
- Fix `test` command not working on `purs` older than `v0.14.0` (#790, #789)

Other improvements:
- Docs: add more useful comments in spago.dhall (#778, 708)
- Dev: remove package.yaml, use only cabal file (#780)
- Dev: use make to orchestrate builds (#781)
- Deps: upgrade to GHC8.10 and lts-17 (#743)

## [0.20.1] - 2021-04-20

Bugfixes:
- Color output now works correctly or is disabled on Windows (#768, #749)
- Fix `spago docs` for PureScript 0.14, by updating `docs-search` to `0.0.11` (#775, #752)

Other improvements:
- Color output is now automatically disabled when output is redirected to a file.
  Also respects a [`NO_COLOR`](https://no-color.org/) environment variable (#768)
- Fixes tests failing if the test platform has `psa` installed (#772)
- Print `spago install` command to fix missing transitive dependencies (#770, #769, #776)
- Refactor the graph support to remove the custom module name parser (#773)

## [0.20.0] - 2021-04-07

Breaking changes (😱!!!):
- `spago build` fails when source files directly import transitive dependencies (#730, #598)

Bugfixes:
- Properly call `psa` to avoid warnings (#730)

Other improvements:
- `spago build` now detects and warns about unused dependencies (#730, #598)

## [0.19.2] - 2021-03-31

New features:
- Allow `verify` and `verify-set` to work with alternate backends, when run in the context of a `spago.dhall` with `backend` set (#754)

Bugfixes:
- Don't fail `bump-version` if the packages don't exist in the Bower registry (#682)

Other improvements:
- CI: bump `purescript` version to 0.14.0 (#759)
- Docs: add FreeBSD installation instructions (#760)
- Docs: clarify description for `--path` flag (#762, #761)

## [0.19.1] - 2021-02-22

Bugfixes:
- Fix `psa` not being found on Windows (#740, #693)
- Use the correct path when erroring out about alternate configurations missing (#746, #747)

Other improvements:
- Bump `dhall` dependency from 1.37.1 to 1.38.0 (#739)
- Fix caching on Windows CI (#728, #741)

## [0.19.0] - 2021-01-06

Breaking changes (😱!!!):
- **Deprecate `-d` flag for `deps-only` (#712)** - instead only support the `--deps-only` long form
- **Switch from `-D` to `-d` as shorthand for specifying dependencies within `spago repl` (#712)**

New features:
- Add `spago script` command. It can be used to run standalone PureScript files as scripts (#712, #724, #683)

Other improvements:
- `spago repl` will no longer create a (mostly unused) full project skeleton in a temporary directory when a config is not found (#712)
- `spago init` and `spago upgrade-set` will now pick the latest set for the available compiler, rathen than just the latest (#721, #687, #657, #666)

## [0.18.1] - 2020-12-22

Breaking changes (😱!!!):
- **Remove `login` command (#705)**

  It was introduced in preparation for a `publish` command that would require a GitHub token to be provided,
  but it is obsolete now, as the new Registry workflow will not require it.
  While it's technically a breaking change, it should be of very low impact since no one should be using this anyways.

- **Upgrade to Dhall 20.0.0 and GHC 8.6.5 (#695, #685)**

  The upgrade fixes several bugs related to the upstream dhall-haskell implementation, but introduces
  a breaking change in the parser, as reserved words are not accepted anymore in certain positions.
  While the upstream package sets have been patched to be compatible with the change, this is a breaking change
  for all the existing configurations that make use of Dhall reserved words (such as `assert`, `let`, etc).

New features:
- Add `exec-args` as alias to `node-args`, to clarify that the args are
  forwarded to whichever backend is being targeted (go, js, etc), not
  exclusively NodeJS (#710, #709)

Bugfixes:
- Don't create the global cache folder at all if the user specifies `--global-cache=skip` (#705, 704)
- Don't require a `spago.dhall` anymore when the `--no-build` flag is passed (#705, 634)

Other improvements:
- CI: switch from Travis to GitHub Actions (#695)

## [0.17.0] - 2020-10-29

Breaking changes (😱!!!):
- **Specify the package set version via `--tag` (#680)**

  Example usage: `spago init --tag psc-0.13.2-20190725` and `spago upgrade-set --tag psc-0.13.2-20190725`.
  This is a breaking change because we are removing support for the old spacchetti/spacchetti-style location (i.e. in the src/packages.dhall) for the upgrade-set command.

Bugfixes:
- Remove dependency on `libtinfo`, removing the biggest cause of friction for using the precompiled binary on various Linux distros (#684)
- Correctly parse flags to be passed to the compiler (#688)

## [0.16.0] - 2020-08-14

Breaking changes (😱!!!):
- **Remove shorthands for `color`, `before`, `then`, and `else` flags (#670, #664)**

  Both `then` and `target` had the shorthand `t`, so the shorthand for `then` was removed.
  Since `then`, `before`, and `else` are all shared between several commands, it seemed
  natural to remove the shorthands for the other shared commands too, so as to not cause
  future shorthand name conflicts.
  A similar collision problem happened with the `color` flag, so its shorthand was removed.
- **Pass main function argument to `--run` for alternate backends (#668)**

  This is a braking change because now alternate backends are expected to accept
  an argument to their `run` flag. This change was coordinated among various backends
  so the migration should be relatively smooth, but you should check if your backend
  it able to support this.

New features:
- Upgrade to `docs-search@v0.0.10`, that introduces grouping by package in local docs (#679)
- Ignore `.gitignore`d files in `--watch` by default - you can disable this with `--allow-ignored` (#665)
- Support `upgrade-set` for alternative package-set repositories (#671)

Bugfixes:
- Make the output of `spago --version` the same with `spago version` (#675)
- Ensure the existence of the global cache directory (#672, #667)

Other improvements:
- Docs: updated package addition/overriding syntax to use `with` syntax (#661, #663)
- Docs: fix Webpack template (#653)
- Docs: document how to pass arguments to `main` (#655)
- Error messages: do not print "Installation complete" if nothing was installed (#676)

## [0.15.3] - 2020-06-15

New features:
- Add `--version` flag to print the version (in addition to the `version` command) (#628)

Bugfixes:
- Actually run `else` commands when `spago run` fails (#632)
- Don't use absolute paths for executables in Windows (#639)

Other improvements:
- Docs: note that `build` also runs `install` (#624)
- Docs: document how to install all the packages in the set (#625)
- Docs: stop suggesting using Parcel and Spago at the same time (#626)
- Docs: document how to install bash and zsh autocompletions (#627)
- Docs: fix explanation in monorepo example (#631)
- Docs: add note about using single quotes for `purs-args` (#637)
- Docs: add quotes for all OSes with `purs-args` (#638)
- Docs: fix typos in `spago bundle-app` help message (#641)
- Deps: upgrade `rio` to `0.1.13.0` (#616)
- CI: upgrade to `purs` v.13.8 (#642)
- CI: stop upgrading broken static binary on release (#620)


## [0.15.2] - 2020-04-15

Breaking changes (😱!!!):
- **Remove the deprecated `--no-share-output` flag (#610)**

  It has been disabled since some time now, so you can just remove it from your build commands.

New features:
- Add the `spago path global-cache` subcommand to output the location of the global cache (#613, #591)

Bugfixes:
- Fix encoding issues causing crashes when running in various locales (#595, #533, #507, #576)
- Respect TERM=dumb by disabling colors when set (#581, #579)
- Run package set commands without a project config being present (#393, #610)
- Fail as soon as possible when not finding a necessary executable (#578, #610)
- Don't exclude the `metadata` package from the set (#609, #610)
- Ensure `psci-support` is installed when starting the repl (#612, #550)
- Ensure dependencies are installed before starting the repl (#611, #610)

Other improvements:
- Errors: make the "dropping the 'purescript-' prefix" warning milder (#571, #570)
- Docs: update README example to include source files (#574)
- Docs: add info about SPDX license for publishing (#606)
- CI: update Travis deployment to `dpl-v2` (#569, #617)
- Deps: upgrade `dhall` to `1.31.1` (#600)
- Curator: move to its own repo (#586)

## [0.14.0] - 2020-02-09

Breaking changes (😱!!!):
- **Replace `list-packages` command with `ls packages` and `ls deps` (#563)**

  This is happening for future extensibility, i.e. so that we can add any
  `spago ls $whatever` subcommand in a non-breaking way whenever we'll want to
  list more things.

  How things got renamed:
  - `spago list-packages` → `spago ls packages`
  - `spago list-packages -f direct` → `spago ls deps`
  - `spago list-packages -f transitive` → `spago ls deps -t`

  Note: the `list-packages` command is still there to provide a semi-gracious transition path for folks, and will be removed in a future release.

New features:
- Allow `verify-set` to work with either a `spago.dhall` or a `packages.dhall` (#515)
- Create `.purs-repl` file when running `spago init` (#555, #558)
- Add `--source-maps` flag to build commands (#545, #562)
- Add `--quiet` and `--no-color` global flags to better control logging (#548)

Bugfixes:
- Fix a few watch-mode buffering and concurrency issues (#549)
- Fix watching relative paths in sources config (e.g. `../src/**/*.purs`) (#556)
- Make the `ensureConfig` function safe (#561, #531)
- Retry downloading packages on network errors (#557, #565)

Other improvements:
- Docs: fix misc typos in README (#566)
- Docs: fix typo in `packages.dhall` template (#539)
- Docs: remove mention of shared output folder in README (#559, #552)
- Docs: update links: spacchetti/spago → purescript/spago
- CI: update to `purs-0.13.6` (#542)
- CI: update CI to the new location of the repo (#560)
- Deps: update to `purescript-docs-search-0.0.8` (#543)
- Deps: update to `dhall-1.29` and `github-0.24` (#553)

## [0.13.1] - 2020-01-10

New features:
- Add `--before`, `--then` and `--else` flags to specify commands to run before and after a build (#532, #410)

Other improvements:
- Docs: fix npm command line argument in README (#597)

## [0.13.0] - 2019-12-19

Breaking changes (😱!!!):
- **Disable `output` folder sharing (#526)**

  This reverts an (accidentally) breaking changes introduced in `0.11.0`, for which
  Spago would take decisions on where the `output` folder should be in order to
  share some compilation results. This turned out to break some other things, so
  we'll stop trying to be smart about it here.

New features:
- Enable HTTP(S) proxies on the NPM installation (#460, #522)

Other improvements:
- Log backend build command when building (#521)
- Deps: update `purescript-docs-search` version to `0.0.6` (#525)
- CI: update `purs` version to `0.13.5` (#513)
- CI: fix Haddocks and start testing them in CI (#511, #516)
- Curator: automate updating `purs` version (#514)
- Curator: automate updating the `purescript-docs-search` version (#519)

## [0.12.1] - 2019-11-17

Bugfixes:
- Fix macOS release artifact (#503, #504)
- Complete parser implementation for module declarations, for `spago test` (#499)

Other improvements:
- Docs: fix typo in README (#498)
- Errors: use `logWarn` for all warnings (#501)

## [0.12.0] - 2019-11-15

Breaking changes (😱!!!):
- **Revert back to dynamically linked binary on Linux (#502, #500, #497)**

  The static binary was still dynamically linking to `glibc`, causing it to be broken on
  some distros. So for now we're back on a dynamically-linked executable.

## [0.11.1] - 2019-11-12

This is identical to `0.11.0`, but published under a new version number due to mishaps in the publishing CI process.

## [0.11.0] - 2019-11-12

Breaking changes (😱!!!):
- **Remove `psc-package`-related commands (#423, #425)**

  Since we are approaching a stable release and `spago` feature set is a superset of `psc-package` ones,
  from this release we do not support the commands to interop with `psc-package`:
  `psc-package-local-setup`, `psc-package-insdhall` and `psc-package-clean` commands.
- **Start sharing the output folder in monorepos, to reduce build duplication (#377, #422)**

  This is a breaking change because your build might stop working if you were relying
  on the `output` folder being in a certain place, and if you were passing `--output`
  as an option to `purs`.
  However, you can pass the `--no-share-output` flag to disable this behavior
- **Build static binaries for Linux (#437, 427)**

  This should fix the dynamic-library-compatibility problems on some distributions.
  It should work as well as the old dynamic binary, but it's theoretically a breaking change since
  some behaviours might be different.
- **Move all logging to `stderr` (#256, #475, #476, #486)**

  All "business output" (e.g. `spago sources`) will stay on `stdout`, so in practice everything
  should be fine, but this is theoretically a breaking change since someone might be depending
  on the output we had so far.


New features:
- add support for `spago build` and `spago run` with alternate backends (#355, #426, #452, #435)

  E.g: add the key `backend = "psgo"` in `spago.dhall` to compile/run with `psgo`
- add new command `spago path` that returns the paths used in the project.

  E.g. `spago path output` returns the output path so that it can be shared with tools such as `purs-loader`. (#463)
- `spago docs` now displays a link to the generated docs' `index.html`, and opens them in the browser when passed the `--open` flag (#379, #421)
- `spago init` has new `--no-comments` flag which skips adding tutorial comments to the generated `spago.dhall` and `packages.dhall` files (#417, #428)
- `spago verify-set` now compiles everything, to detect duplicate module names. This can be disabled with `--no-check-modules-unique` (#438)
- `spago install purescript-XYZ` will now strip `purescript-` prefix and install XYZ (if it exists in package set) instead of just failing with a warning (#367, #443)
- `spago run` now allows to pipe `stdin` to your running project (#488, #490)

Bugfixes:
- Fix Ctrl-C handling in REPL when using NPM installation on Windows (#493, #483)
- Fix confusing warning when trying to `spago install` a package already present in project dependencies list (#436, #439)
- Warn (but don't error) when trying to `--watch` missing directories (#406, #420, #447, #448)
- Do not watch files in `.spago` folder when running with `--watch` (#430, #446)
- The `--clear-screen` flag (usable e.g. with `spago build --watch`) now also resets cursor position, so the rebuild message always appears at top left of the screen (#465, #466)
- Allow additional fields in the config for local packages (#470)
- Fix `--config` option: get the correct paths when config file is in another directory (#478, #484)

Other improvements:
- Tests: speed up test suite by replacing some end-to-end tests with unit/property tests (#445, #440)
- Tests: update instructions to run tests (#449)
- Tests: always run test suites with UTF8 encoding (#482)
- Docs: various improvements to README (#432, #457, #464, #487)
- Docs: add "getting started" guides for Parcel, Webpack and Nodemon (#456, #461, #473)
- Errors: improve cache skipping error (#453, #480, #481)
- Errors: add a nice error message when trying to run `spago test` with no test modules (#489, #383, #492)
- Refactor: fix `hlint` warnings (#450)
- Refactor: rewrite Curator for moar maintainability (#458, #419)
- Deps: update to Dhall 1.27 and Purs 0.13.4 (#469)
- Deps: revert to GHC 8.4.4 and LTS-12 (#479)
- CI: fix release code (#494, #495)


## [0.10.0] - 2019-09-21

Breaking changes (😱!!!):
- **Flags and arguments that you want to give to `purs` are now passed with `--purs-args` (#353, #366)**

  The previous behaviour in which all arguments that could not parse as `spago` arguments
  were passed along to `purs` was sometimes confusing (e.g. when using `--path` and multiple
  arguments).

New features:
- Support watching js files (#407, #205)
- New `--no-search` flag for `spago docs` to skip patching the documentation using `purescript-docs-search` (#400)
- New `-x` flag for specifying the config path location (#357, #329)
- New `spago login` command, to save a GitHub token to the cache so it can be used for various operations hitting GitHub (#391, #403)

Bugfixes:
- "Quit" command in watch mode now actually quits (#390, #389)
- Do not compile files twice when using `--watch` and Vim (#346, #371)
- Use `git clone` instead of `git fetch` when fetching a package, so all tags can be installed (#373, #374)
- Fix Windows global cache location; now uses `LocalAppData` as default (#384, #380)
- Fix naming clash in short flag for repl dependencies (#352, #350)
- Fix failure to copy to global cache on a different filesystem (#385, #386)
- Fix watch function on Windows (issue with paths) (#387, #380, #401)
- Look up remote imports dynamically when doing frozen check, to always find the right `packages.dhall` (#349, #402)

Other Improvements:
- Performance: make no-op `spago install` faster (#409, #412)
- CI: remove reviews limitation on mergify (#354)
- CI: various fixes (#362, #368, #382, #388, #418)
- Docs: fix syntax errors in template comment (#369, #413, #408)
- Docs: fix link for package-set from commit (#405)
- Docs: keep README up to date with new features (#398, #347)
- Deps: upgrade to lts-14 and GHC-8.6 (#395)
- Deps: upgrade to dhall-1.26.0, v10 of the standard (#411, #358)

## [0.9.0] - 2019-07-30

Breaking changes (!!!):
- **Rename `package-set-upgrade` to `upgrade-set` (#336)**

  You now have to call `spago upgrade-set` if you wish to upgrade your package-sets version

- **Move the `--jobs` flag to be global (#338)**

  If you were invoking spago in this way: `spago install -j 10`, you now have to use `spago -j 10 install` instead

- **Import local packages `as Location` (#301, #244)**

  Before you'd import a local package in this way:

  ```dhall
  let additions =
    { foobar =
        mkPackage
          (../foobar/spago.dhall).dependencies
          "../foobar"
          "local-fix-whatever"
    }
  ```

  ..but now you'll have to import it using `as Location` instead:

  ```dhall
  let additions =
    { foobar = ../foobar/spago.dhall as Location }
  ```


New features:
- Add searchbar to docs generated with `spago docs` (#340, #333, #89)
- Add automatic migration of Bower projects when doing `spago init` (#159, #272, #342)
- Add `bump-version` command, for generating `bower.json` files and making version tags in Git (#203, #289, #324)
- Use `psa` for compiling if installed; you can avoid this with the new `--no-psa` flag (#305, #283, #252, #327)
- Add support for starting a repl within a folder which has not been setup as a spago project (#168, #280)
- Add `--format` flag to `spago docs` (#294, #299)
- Add project sources to `spago sources` output (#276, #287, #308)
- Watch all sources, including dependencies, when building with filewatch (#172, #309)
- Add `--deps-only` flag to build dependencies alone (#330, #331)

Bugfixes:
- Fix `spago install` failing when version branch names differ only by case on case-insensitive filesystems (#285)
- Change `--node-args` shortcut to `-a` to avoid clash (#292, #293)
- Stop reformatting config files if not necessary (#300, #302, #339)
- Make `spago run` write a file and execute it so that args are passed correctly (#297, #295)
- Add fallback for global cache directory (#314, #312)
- Do not overwrite `spago.dhall` when doing `spago init` twice (#318, #321)
- Catch exceptions when trying to fetch metadata (#325)
- Generate hashes when doing `psc-package-insdhall` (#337, #240)

Other Improvements:
- Curator: log exceptions to file to monitor eventual issues (#284)
- Docs: update README with newest features (#286)
- Docs: add docs about switching from Bower (#317)
- Errors: improve error message for overriding compiler version (#345, #343)
- Tests: improve failure messages (#298)
- Tests: fix `packages.dhall` fixtures manipulation (#307)
- Tests: add tests for the `list-packages` command (#304)
- Tests: add tests for local dependencies (#310)
- Config: remove `mkPackage` function in Dhall configs, and switch to package-sets releases for upstream (#322, #320, #319)
- Config: update test template to use `Effect.Class.Console` (#328, #334)
- CI: fix missing "commit since last release" message (#326)
- CI: add configuration for Mergify (#332)


## [0.8.5] - 2019-06-18

ZuriHac edition 🎉

New features:
- Add `sources` key to config to customize the sources used in the build (#273, #173)
- Add `--json` flag to the `list-packages` command to optionally output JSON (#263)
- Add `--clear-screen` flag to to clear the screen when watching (#271, #209)
- Add `--no-install` flag for build to prevent automatic installation (#274, #269)
- Add `--node-args` flag to pass arguments to Node in `run/test` commands (#267, #275)

Bugfixes:
- Fix `spago install` failing when version branch name contains `/`'s (#257, #258)
- Report all missing packages together when it's not possible to build an install plan (#264, #223)
- Pull the latest package-sets version when doing `init` (#254, #279)
- Fix `spago install` not adding new dependencies when list is empty (#282, #281)

Other Improvements:
- Docs: add visual overview of what Spago does "under the hood" in typical project workflow (#211)
- Docs: fix outdated references in README (#266)
- Tests: untangle testcases environments (#265, #214)
- Tests: improve packages test by checking for missing and circular dependencies (#270)

## [0.8.4] - 2019-06-12

New features:
- Add option to clear the screen to spago build/run (#209)
- Add option to pass args to node when doing spago test/run (#267)

Bugfixes:
- Produce an error message when asserting directory permissions (#250)
- Read purs version from inside the set instead of its GitHub tag (#253, #225)
- Skip copy to global cache when encountering a permissions problem (#220, #260)

Other improvements:
- Errors: add many debug logs (#251)
- CI: rewrite Curator in Haskell (#239)
- CI: build only `master` and tags on Travis (#247)
- Dev: add Nix section to stack.yaml (#248)
- Dev: tidy up the various executables, sources and dependencies (#251)

## [0.8.3] - 2019-06-03

Bugfixes:
- Fix `spago psc-package-clean` on Windows (#224)
- Fix `spago repl` starting on Windows where PureScript was installed with NPM (#235, #227)
- Fix missing filenames when encountering parse errors in Dhall files (#241, #222)
- Download packages in local repo instead of global tempdir (#243, #220)

Other improvements:
- Tests: test suite now works fully on Windows (#224)
- CI: parametrize LTS version (#236)
- CI: get PureScript binary for Travis from GitHub releases (#234)
- Error messages: fix whitespace (#221)

## [0.8.1] - 2019-05-29

New features:
- Add global cache to avoid redownloading dependencies (#188, #133)
- Add ability to pin a version to a commit hash in addition to branches and tags (#188, #200)

Bugfixes:
- Another attempt to fix NPM and Yarn installations on Windows (#215, #187)

Other improvements:
- The test suite is now written in Haskell rather than Python (#212, #177)
- Add `spago-curator` tool to generate metadata from the package set (#202)
- Improve docs (#208, #207, #218, #217)

## [0.8.0] - 2019-05-16

Breaking changes:
- Rename "bundle" to "bundle-app" and "make-module" to "bundle-module" for consistency (#175, #147)

Bugfixes:
- Don't fail `init` if a `packages.dhall` is already there, as it's the case of psc-package projects with local spacchetti (#180)

Other improvements:
- Remove CI check for package-sets version, add cron script to update it instead (#185)
- Fill in CHANGELOG from release notes (#186)
- Fix LICENSE file so GitHub recognizes it (#197)
- Add a CONTRIBUTING file (#198, #189)
- Improve README (#199, #194, #196, #201, #193, #192, #187, #178, #191, #150, #142)

## [0.7.7] - 2019-04-28

New features:
- Install "psci-support" on project init (#174)

## [0.7.5] - 2019-03-30

Bugfixes:
- Fix NPM and Yarn installations on Linux and Windows (#157, #167, #166)

## [0.7.4] - 2019-03-27

Bugfixes:
- correctly parse package-set release tag to avoid generating unnecessary warnings (#160, #161)
- skip 0.7.3.0 as I forgot to update the version field (#164)

## [0.7.2] - 2019-03-21

New features:
- introduce a `--verbose` flag to print debug information - e.g. `purs` commands being called by Spago (#154, #155)

## [0.7.1] - 2019-03-19

New features:
- Add `--watch` flag to `build`, `test`, `run`, `bundle` and `make-module` commands (#65, #126, #153)
- Add `spago docs` command, to generate documentation from the project and all dependencies (#127)
- Add `spago run` command, to run your project (#131, #137)

Other fixes and improvements:
- Automatically build in commands that require the project to be built (#146, #149)
- Don't automatically create a configuration if not found (#139, #144)
- Always ensure that the package-set has a hash on it, for speed and security reasons (#128)
- Improvements to documentation and FAQ (#132, #125, #119, #123, #104, #135)
- Improvements to errors, messages and logging (#143, #145, #133, #151, #148, #129, #130, #136)
- Improvements to tests (#95, #91, #138, #141, #140)
- Format Dhall files with ASCII instead of Unicode (#124)

## [0.7.0] - 2019-03-03

Breaking changes:
- The NPM package `purescript-spago` is now deprecated. New releases will be published only to the package `spago` (#115, #44)
- [Spacchetti has been merged in the official package-set](https://github.com/purescript/package-sets/pull/271): this means that `spago` will now use that as the reference package-set. (#120)

  As a result of this, the command `spago spacchetti-upgrade` has been renamed to `spago package-set-upgrade`.

New features:
- Support Windows in NPM install (#121, #109)
- Add `spago freeze` command to recompute hashes of the package-set (#113)
- Add `spago verify` and `spago verify-set` commands (#108, #14)
- Add the `--filter` flag to `spago list-packages`, to filter by direct and transitive deps (#106, #108)
- Check that the version of the installed compiler is at least what the package-set requires (#101, #107, #117, #116)

Other improvements:
- Improve the installation: do less work and print less useless stuff (#110, #112, #114)
- Skip the copy of template files if the source directories exist (#102, #105)

## [0.6.4] - 2019-02-07

New features:
- [`spago init` will search for a `psc-package.json`, and try to port it to your new `spago.dhall` config](https://github.com/purescript/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#switching-from-psc-package) (#76)
- [Add the `spacchetti-upgrade` command, to automatically upgrade to the latest Package Set](https://github.com/purescript/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#upgrading-the-package-set) (#93, #73)
- [You can now add local packages to the Package Set 🎉](https://github.com/purescript/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#adding-and-overriding-dependencies-in-the-package-set) (#96, #88)
- [Now it's possible to run `spago install foo bar` to add new dependencies to your project](https://github.com/purescript/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#adding-a-dependency) (#74)
- Now every time you try to build, Spago will also check that dependencies are installed (#75, #82)

Bugfixes:
- Spago would crash if `$HOME` was not set, now it doesn't anymore (#85, #90)
- `spago test` now actually works on Windows (#79)

Other improvements:
- Maany docs improvements (#83, #76, #93, #96, #99, #100)
- From this release we are publishing an experimental Windows build (#81)
- Add a PR checklista, so we don't forgetti (#86)

## [0.6.3] - 2019-01-18

New features:
- `spago repl` will now spawn a PureScript repl in your project (#46, #62)
- `spago list-packages` will list all the packages available in your package-set (#71)

## [0.6.2] - 2019-01-07

New features:
- `spago build` and `spago test` now have the `--path` option to specify custom source paths to include (#68, #69)
- `spago build` and `spago test` can now pass options straight to `purs compile` (#66, #49)

## [0.6.1] - 2018-12-26

New features:
- Add initial windows support (#47, #48, #58): now `spago` should run fine on Windows. Unfortunately we're not distributing binaries yet, but the only installation method available is from source (with e.g. `stack install`)

Bugfixes:
- Don't overwrite files when doing `init`, just skip the copy if some file exists (#56)
- Print `git` output in case of failure when doing `install` (#54, #59)
- Include building `src/*` when running `test` (#50, #53)
- Make file embedding indipendent of the locale when compiling; now we just use Unicode

## [0.6.0] - 2018-12-16

First release under the name "spago".

Main changes from the previous "spacchetti-cli" incarnation:
- Rename `spacchetti-cli` → `spago` (#23)
- Publish on NPM under the new name `purescript-spago` (#35)
- Add some commands from `psc-package`:
  - `init` (#12): initialize a new sample project with a `spago.dhall` and a `packages.dhall` config files
  - `install` (#11, #32): concurrently fetch dependencies declared in `spago.dhall` file
  - `sources` (#13): print source globs
  - `build`: compile the project with `purs`
- Migrate old commands from `spacchetti-cli` that are specific to local psc-package projects:
  - `local-setup` is now `psc-package-local-setup`
  - `insdhall` is now `psc-package-insdhall`
  - `clean` is now `psc-package-clean`
- Add some commands from `purp` (#26):
  - `test`: compile and run a module from the `test/` folder
  - `bundle`: bundle all sources in a single file
  - `make-module`: export the above bundle so it can be `require`d from js
- Stop depending on `dhall` and `dhall-to-json` commands and instead depend on `dhall` and `dhall-json` libraries
- Freeze `spacchetti` package-set import in `packages.dhall`, so `dhall` caching works for subsequent executions
- Move to v4.0.0 of `dhall`
- Add integration tests for most of the commands (#31, #30)
