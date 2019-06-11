# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.4] - 2019-06-11

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
  You can install the package with `npm install -g spago`
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
- [`spago init` will search for a `psc-package.json`, and try to port it to your new `spago.dhall` config](https://github.com/spacchetti/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#switching-from-psc-package) (#76)
- [Add the `spacchetti-upgrade` command, to automatically upgrade to the latest Package Set](https://github.com/spacchetti/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#upgrading-the-package-set) (#93, #73)
- [You can now add local packages to the Package Set 🎉](https://github.com/spacchetti/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#adding-and-overriding-dependencies-in-the-package-set) (#96, #88)
- [Now it's possible to run `spago install foo bar` to add new dependencies to your project](https://github.com/spacchetti/spago/tree/6947bf1e9721b4e8a5e87ba8a546a7e9c83153e9#adding-a-dependency) (#74)
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
