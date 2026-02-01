# Contributing to Spago


## Do I belong here?

Everybody is welcome! People of all experience levels can join and begin contributing and
should feel comfortable and safe making mistakes.

People of all backgrounds belong here as long as they treat others with dignity
and respect and do not harass or belittle others.


## What is the correct way to ask a question?

It's ok to ask questions by [opening an issue][spago-issues]!
I ([@f-f][f-f]) am also on the [PureScript Discord][discord], so you can also ask
questions in the `#spago` channel.


## I'd like to help, how do I pick something to work on?

Any [open issue][spago-issues] that is not assigned to anyone is good to work on!

If it's your first time contributing then it's probably best to pick the ones marked
with "good first issue".

The easiest way you can help is by contributing documentation (look for issues with
the label "document me").

Do not worry if it's just about copypasting some instructions from an issue to the README,
everything is welcome!

If you wish to contribute documentation, [this is a suggested read](https://www.divio.com/blog/documentation/)

## Developing `spago`

```bash
# Install dependencies
npm ci

# Bootstrap
spago bundle -p spago-bin

# From now on you can build with the local files in the output folder, e.g.:
./bin/index.dev.js bundle -p spago-bin
# Or from the built bundle:
./bin/bundle.js bundle -p spago-bin

# From now on you can use the bootstrapped build to see the changes you make:
./bin/index.dev.js build --pedantic-packages --some-new-build-flag

# ...but you can of course still just use the global `spago` command:
spago build

# Can of course run the tests with
spago test

# Can select a subset of tests to run
spago test -- --example "bundle" # run only bundle tests
spago test -- --example "browser" # run only tests that mention browser in their name

# Can select a subset of tests by a regular expression
spago test -- --example-matches "bundle|browser" # run bundle tests _and_ those that mention browser

# To see tests' stdout/stderr output while the tests are running, run
SPAGO_TEST_DEBUG=1 spago

# To overwrite fixture's with the test's run, run
SPAGO_TEST_ACCEPT=1 spago
```

## Developing docs

If you edit any title in the readme, run `doctoc` to update the Table of Contents:

```bash
doctoc --notitle README.md
```

If you want to contribute new or edit existing flowcharts, download [yEd](https://www.yworks.com/products/yed), use that program to edit the `.graphml` files, and then export them as an SVG or PNG file via the <kbd>CTRL+E</kbd> shortcut. When a dialog box appears, just press 'ok' and use the default export settings.

The following are recommendations to make using `yEd` easier/faster:
- In the 'Preferences' dialog
    - Under the 'General' tab, change the "Mouse Wheel Behavior" to "[Wheel] Up/Down, [Shift+Wheel] Left/Right, [Ctrl+Wheel] In/Out"
    - Under the 'Editor' tab, check the "Create Node on background click" box
    - Under the 'Editor' tab, check the "Edit Label on Create Node" box
    - Under the 'Editor' tab, check the "Dynamically Adjust Node Size to Label Size" box
- Click on an edge between two nodes...
    - ... and press <kbd>F2</kbd> to add text to that edge
    - ... and press <kbd>F6</kbd> to edit its properties. Under the 'Label' tab, change the "Placement"'s "Model" dropdown to "Free" to get full control over where the edge's text can appear.

## Merging changes

All changes must happen through a Pull Request.

Everyone with the "commit bit" can merge changes.

## How do I get the "commit bit"?

Just ask!  I ([@f-f][f-f]) hand out the commit bit freely to anybody who
displays sustained interest in the project, sometimes even if they don't ask.
However, you shouldn't assume that you are undeserving if I haven't given you
the commit bit.  I might just be busy or forgetful, so I appreciate a helpful
reminder.  You can ask directly by e-mail (see my account profile) or by opening
an issue, whichever you prefer.

I hand out the commit bit freely because mistakes are easy to fix or roll back.
Learn by doing and get your hands dirty!

## Releasing

Releases are cut from the `master` branch using the `release.sh` script.

### Pre-release checklist

1. Ensure all changes are merged to `master`
2. Update `CHANGELOG.md`: move items from `[Unreleased]` to a new version section
3. Verify tests pass: `spago test`
4. Verify the build works: `spago build`

### Cutting a release

```bash
./release.sh <version>
```

Examples:
```bash
./release.sh 1.0.0    # explicit version
./release.sh patch    # bump patch (e.g., 1.0.0 -> 1.0.1)
./release.sh minor    # bump minor (e.g., 1.0.0 -> 1.1.0)
```

The script will:
1. Verify you're on `master` with a clean working tree
2. Pull the latest changes
3. Bump the version in `package.json` and sync it to `spago.yaml` files
4. Commit and create a git tag
5. Push to origin

### What happens next

CI (`.github/workflows/release.yml`) automatically:
1. Builds and tests the project
2. Creates a GitHub Release
3. Publishes to npm

[f-f]: https://github.com/f-f
[discord]: https://purescript.org/chat
[spago-issues]: https://github.com/purescript/spago/issues

## Working with file paths

File paths are very important in Spago. A very big chunk of Spago does is
shuffling files around and manipulating their paths. Representing them as plain
strings is not enough.

Spago has three different kinds of paths, represented as distinct types:

- `RootPath` can generally be the root of anything, but in practice it usually
  points to the root directory of the current workspace. It is constructed in
  `Main.purs` close to the entry point and is available in all `ReaderT`
  environments as `rootPath :: RootPath`
- `LocalPath` is path of a particular file or directory within the workspace. It
  doesn't have to be literally _within_ the workspace directory - e.g. a custom
  dependency that lives somewhere on the local file system, - but it's still
  _relative_ to the workspace. A `LocalPath` is explicitly broken into two
  parts: a `RootPath` and the "local" part relative to the root. This is useful
  for printing out workspace-relative paths in user-facing output, while still
  retaining the full path for actual file operations. A `LocalPath` can be
  constructed by appending to a `RootPath`. Once so constructed, the `LocalPath`
  always retains the same root, no matter what subsequent manipulations are done
  to it. Therefore, if you have a `LocalPath` value, its root is probably
  pointing to the current workspace directory.
- `GlobalPath` is for things that are not related to the current workspace.
  Examples include paths to executables, such as `node` and `purs`, and global
  directories, such as `registryPath` and `globalCachePath`.

Paths can be appended by using the `</>` operator. It is overloaded for all
three path types and allows to append string segments to them. When appending to
a `RootPath`, the result comes out as `LocalPath`. You cannot produce a new
`RootPath` by appending.

Most code that deals with the workspace operates in `LocalPath` values. Most
code that deals with external and global things operates in `GlobalPath` values.
Lower-level primitives, such as in the `Spago.FS` module, are polymorphic and
can take all three path types as parameters.

For example:

```haskell
rootPath <- Path.mkRootPath =<< Paths.cwd
config <- readConfig (rootPath </> "spago.yaml")
let srcDir = rootPath </> "src"
compileResult <- callCompiler [ srcDir </> "Main.purs", srcDir </> "Lib.purs" ]
FS.writeFile (rootPath </> "result.json") (serialize compileResult)
```
