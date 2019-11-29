# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.6 - 2019-11-29]

New features:
- Allow using the app as a search engine (show search term in URI hash) (#26)

## [0.0.5 - 2019-09-21]

New features:
- Render docs as markdown (#15)
- Show help for each CLI command (#17)
- Add packages to the search index (#16)
- Sort search results by popularity (based on number of package reverse dependencies).
- Add `--no-patch` flag to `build-index` command.

Bugfixes:
- Fix decoding of kind annotations in `forall`s (#17)
- Fix rendering for variable-parametrized records (e.g. `Record a`) and type-level strings.

## [0.0.4] - 2019-07-25

New features:
- Reduce the package size by using a minifier.
- Add `version` command to print the app version.
- **S** hotkey now `.select()`s everything in the search field, insetead of just `.focus()`ing (#11).

## [0.0.3] - 2019-07-23

Bugfixes:
- Fix stack safety issue (#8).

## [0.0.2] - 2019-07-21

## [0.0.1] - 2019-07-20
