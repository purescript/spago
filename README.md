# spago

[![npm](https://img.shields.io/npm/v/spago.svg)][spago-npm]
[![Build Status](https://travis-ci.com/spacchetti/spago.svg?branch=master)][travis-spago]
[![Build status](https://ci.appveyor.com/api/projects/status/jydvr4sur6j6816e/branch/master?svg=true)](https://ci.appveyor.com/project/f-f/spago/branch/master)

*(IPA: /ˈspaɡo/)*

PureScript package manager and build tool powered by [Dhall][dhall] and
[package-sets][package-sets].


<img src="https://raw.githubusercontent.com/spacchetti/logo/master/spacchetti-icon.png" height="300px" alt="spacchetti logo">

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Design goals and reasons](#design-goals-and-reasons)
  - [Brief survey of other package managers and build tools available](#brief-survey-of-other-package-managers-and-build-tools-available)
- [Developing and contributing](#developing-and-contributing)
- [Installation](#installation)
- [Super quick tutorial](#super-quick-tutorial)
- [How do I...](#how-do-i)
  - [Switch from `psc-package`](#switch-from-psc-package)
  - [See what commands and flags are supported](#see-what-commands-and-flags-are-supported)
  - [Download my dependencies locally](#download-my-dependencies-locally)
  - [Build and run my project](#build-and-run-my-project)
  - [Test my project](#test-my-project)
  - [Run a repl](#run-a-repl)
  - [List available packages](#list-available-packages)
  - [Add a direct dependency](#add-a-direct-dependency)
  - [Override a package in the package set with a local one](#override-a-package-in-the-package-set-with-a-local-one)
  - [Override a package in the package set with a remote one](#override-a-package-in-the-package-set-with-a-remote-one)
  - [Add a package to the package set](#add-a-package-to-the-package-set)
  - [`bower link`](#bower-link)
  - [Verify that an addition/override doesn't break the package set](#verify-that-an-additionoverride-doesnt-break-the-package-set)
  - [Automagically upgrade the package set](#automagically-upgrade-the-package-set)
  - [Separate `devDependencies` or test dependencies](#separate-devdependencies-or-test-dependencies)
  - [Bundle a project into a single JS file](#bundle-a-project-into-a-single-js-file)
    - [1. `spago bundle-app`](#1-spago-bundle-app)
    - [2. `spago bundle-module`](#2-spago-bundle-module)
    - [Skipping the Build Step](#skipping-the-build-step)
  - [Make a project with PureScript + JavaScript](#make-a-project-with-purescript--javascript)
  - [Generate documentation for my project](#generate-documentation-for-my-project)
  - [Publish my library](#publish-my-library)
  - [Use this together with `psc-package`](#use-this-together-with-psc-package)
  - [Get all the licenses of my dependencies](#get-all-the-licenses-of-my-dependencies)
  - [Know what `purs` commands are run under the hood](#know-what-purs-commands-are-run-under-the-hood)
  - [Ignore or update the global cache](#ignore-or-update-the-global-cache)
- [Explanations](#explanations)
  - [Visual Overview: What happens when you do 'spago build'?](#visual-overview-what-happens-when-you-do-spago-build)
  - [Configuration file format](#configuration-file-format)
  - [Why can't `spago` also install my npm dependencies?](#why-cant-spago-also-install-my-npm-dependencies)
  - [Why we don't resolve JS dependencies when bundling, and how to do it](#why-we-dont-resolve-js-dependencies-when-bundling-and-how-to-do-it)
  - [How does the "global cache" work?](#how-does-the-global-cache-work)
- [Troubleshooting](#troubleshooting)
    - [I added a git repo URL to my overrides, but `spago` thinks it's a local path 🤔](#i-added-a-git-repo-url-to-my-overrides-but-spago-thinks-its-a-local-path-)
    - [My `install` command is failing with some errors about "too many open files"](#my-install-command-is-failing-with-some-errors-about-too-many-open-files)
    - [Package set caching problems](#package-set-caching-problems)
    - [I added a new package to the `packages.dhall`, but `spago` is not installing it. Why?](#i-added-a-new-package-to-the-packagesdhall-but-spago-is-not-installing-it-why)
- [Reference - Internals](#internals)
  - [The `spago-curator` tool](INTERNALS.md#the-spago-curator-tool)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


## Design goals and reasons

Our main design goals are:
- **Great UX**: you're not supposed to spend your life configuring the build for your project.
  A good build system just does what's most expected and gets out of the way so you can focus
  on actually writing the software.
- **Minimal dependencies**: users should not be expected to install a myriad of tools on their
  system to support various workflows. We depend only on `git` and `purs` being installed.
- **Reproducible builds**: thanks to [package sets][package-sets] and [Dhall][dhall], if your
  project builds today it will also build tomorrow and every day after that.

Some tools that inspired `spago` are: [Rust's Cargo][cargo], [Haskell's Stack][stack], 
[`psc-package`][psc-package], [`pulp`][pulp] and [`purp`][purp].


### Brief survey of other package managers and build tools available

`pulp` is excellent, but it is only a build tool. This means that you'll have to use it with
either `bower` or `psc-package`:
- If you go for `bower`, you're missing out on package-sets (that is: packages versions
  that are known to be working together, saving you the headache of fitting package
  versions together all the time).
- If you use `psc-package`, you have the problem of not having the ability of overriding
  packages versions when needed, leading everyone to make their own package-set, which
  then goes unmaintained, etc.
  
  Of course you can use the package-set-local-setup to solve this issue, but this is
  exactly what we're doing here: integrating all the workflow in a single tool, `spago`,
  instead of having to install and use `pulp`, `psc-package`, `purp`, etc.


## Developing and contributing

We'd love your help, and welcome PRs and contributions!

Some ideas for getting started:
- [Build and run `spago`](CONTRIBUTING.md#developing-spago)
- [Help us fix bugs and build features](https://github.com/spacchetti/spago/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+label%3A%22defect%22)
- Help us improve our documentation
- Help us [log bugs and open issues][new-issue]

For more details see the [`CONTRIBUTING.md`][contributing]


## Installation

The recommended installation method for Windows, Linux and macOS is `npm` (see the latest releases on npm
  [here][spago-npm]):
  
```
npm install -g spago
``` 

Other installation methods available:
- Download the binary from the [latest GitHub release][spago-latest-release]
- Compile from source by cloning this repo and running `stack install`
- With Nix, using [easy-purescript-nix][spago-nix]

**General notes:**
- The assumption is that you already installed the [PureScript compiler][purescript].
  If not, get it with `npm install -g purescript`, or the recommended method for your OS.
- You might have issues with `npm` and Docker (e.g. getting the message "Downloading the spago binary failed.." etc)
  You have two options:
  - either **do not run npm as root**, because it doesn't work well with binaries. Use it as a nonprivileged user.
  - or use `--unsafe-perm`: `npm install -g --unsafe-perm spago` 

**Notes for Linux users:**
- If you get networking errors (e.g. "Host Not Found") you may need to install `netbase`.
  For more details see [this issue][ubuntu-issue-netbase].
- If you encounter issues with `libtinfo.so.5`, see [this issue][ubuntu-issue-libtinfo] for a fix.

**Notes for NixOS users**: as you might expect, the `npm` installation won't work because it's
dynamically linked. Use [easy-purescript-nix][spago-nix].


## Super quick tutorial

Let's set up a new project!

```bash
$ mkdir purescript-unicorns
$ cd purescript-unicorns
$ spago init
```

This last command will create a bunch of files:

```
.
├── packages.dhall
├── spago.dhall
├── src
│   └── Main.purs
└── test
    └── Main.purs
```

Let's take a look at the two [Dhall][dhall] configuration files that `spago` requires:
- `packages.dhall`: this file is meant to contain the *totality* of the packages
  available to your project (that is, any package you might want to import).
  
  In practice it pulls in the [official package-set][package-sets] as a base,
  and you are then able to add any package that might not be in the package set,
  or override existing ones.
- `spago.dhall`: this is your project configuration. It includes the above package-set,
  the list of your dependencies, the source paths that will be used to build, and any
  other project-wide setting that `spago` will use.

To build your project, run:

```bash
$ spago build
```

This will download the necessary dependencies and compile the sample project in the `output/`
directory. You can take a look at the content of `output/Main/index.js` to see what kind
of JavaScript has been generated from your new `Main.purs` file.

You can already see your project running, by doing

```bash
$ spago run
```

..which is basically equivalent to the following command:

```bash
$ node -e "require('./output/Main/index').main()"
```

..which imports the JS file you just looked at, and runs the `main` with Node.

You can also bundle the project in a single file with an entry point, so it can be run directly (useful for CLI apps):

```bash
$ spago bundle-app
$ node .
```


## How do I...

This section contains a collection of workflows you might want to use to get things done with `spago`

### Switch from `psc-package`

Do you have an existing `psc-package` project and want to switch to `spago`?

No problem! If you run `spago init`, we'll port your existing `psc-package.json`
configuration into a new `spago.dhall` 😎

Note: `spago` won't otherwise touch your `psc-package.json` file, so you'll have to
remove it yourself.

You'll note that most of the `psc-package` commands are the same in `spago`, so porting
your existing build is just a matter of search-and-replace most of the times.


### See what commands and flags are supported

For an overview of the available commands, run:

```bash
$ spago --help
```

You will see several subcommands (e.g. `build`, `test`); you can ask for help
about them by invoking the command with `--help`, e.g.:

```bash
$ spago build --help
```

This will give a detailed view of the command, and list any command-specific
(vs global) flags.


### Download my dependencies locally

```bash
$ spago install
```

This will download all the transitive dependencies of your project (i.e. the direct dependencies,
i.e. the ones listed in the `dependencies` key of `spago.dhall`, plus all their dependencies, 
recursively) to the local `.spago` folder (and the global cache, if possible).

However, running this directly is usually **not necessary**, as all commands that need the dependencies
to be installed will run this for you.

### Build and run my project

We can build the project and its dependencies by running:

```bash
$ spago build
```

This is just a thin layer above the PureScript compiler command `purs compile`.

The build will produce very many JavaScript files in the `output/` folder. These
are CommonJS modules, and you can just `require()` them e.g. on Node.

It's also possible to include custom source paths when building (`src` and `test` 
are always included):

```bash
$ spago build --path 'another_source/**/*.purs'

```

**Note**: the wrapper on the compiler is so thin that you can pass options to `purs`.
E.g. if you wish to output your files in some other place than `output/`, you can run

```bash
$ spago build -- -o myOutput/
```

If you wish to automatically have your project rebuilt when making changes to source files
you can use the `--watch` flag:

```bash
$ spago build --watch

# or, to clear the screen on rebuild:
$ spago build --watch --clear-screen
```

If you want to run the program (akin to `pulp run`), just use `run`:
```bash
# The main module defaults to "Main"
$ spago run

# Or define your own module path to Main
$ spago run --main ModulePath.To.Main

# And pass arguments through to `purs compile`
$ spago run --main ModulePath.To.Main -- --verbose-errors

# Or pass arguments to node
$ spago run --node-args "arg1 arg2"
```


### Avoid re-formatting the `spago.dhall` and `packages.dhall` with each command

You can pass the `--no-config-format` or `-F` global flag:

``` bash
$ spago build -F
Installation complete.
Build succeeded.
```


### Test my project

You can also test your project with `spago`:

```bash
# Test.Main is the default here, but you can override it as usual
$ spago test --main Test.Main
Build succeeded.
You should add some tests.
Tests succeeded.
```


### Run a repl

As with the `build` and `test` commands, you can add custom source paths
to load, and pass options to the underlying `purs repl` by just putting
them after `--`.

E.g. the following opens a repl on `localhost:3200`:

```bash
$ spago repl -- --port 3200
```


### List available packages

It is sometimes useful to know which packages are contained in our package set
(e.g. to see which version we're using, or to search for packages).

You can get a complete list of the packages your `packages.dhall` imports (together
with their versions and URLs) by running:

```bash
$ spago list-packages
```

By passing the `--filter` flag you can restrict the list to direct or transitive dependencies:

```bash
# Direct dependencies, i.e. only the ones listed in spago.dhall
$ spago list-packages --filter=direct

# Transitive dependencies, i.e. all the dependencies of your dependencies
$ spago list-packages -f transitive
```


### Add a direct dependency

You can add dependencies that are available in your package set by running:

```bash
# E.g. installing Halogen
$ spago install halogen

# This also supports multiple packages
$ spago install foreign simple-json
```


### Override a package in the package set with a local one

Let's say I'm a user of the `simple-json` package. Now, let's say I stumble upon a bug
in there, but thankfully I figure how to fix it. So I clone it locally and add my fix.

Now if I want to test this version in my current project, how can I tell `spago` to do it?

We have a `overrides` record in `packages.dhall` just for that!

In this case we override the `repo` key with the local path of the package.
It might look like this:

```haskell
let overrides =
      { simple-json =
            upstream.simple-json // { repo = "../purescript-simple-json" }
      }
```

Note that if we `list-packages`, we'll see that it is now included as a local package:

```bash
$ spago list-packages
...
signal                v10.1.0   Remote "https://github.com/bodil/purescript-signal.git"
sijidou               v0.1.0    Remote "https://github.com/justinwoo/purescript-sijidou.git"
simple-json           v4.4.0    Local "../purescript-simple-json"
simple-json-generics  v0.1.0    Remote "https://github.com/justinwoo/purescript-simple-json-generics.git"
smolder               v11.0.1   Remote "https://github.com/bodil/purescript-smolder.git"
...
```

And since local packages are just included in the build, if we add it to the `dependencies`
in `spago.dhall` and then do `spago install`, it will not be downloaded:

```
$ spago install
Installing 42 dependencies.
...
Installing "refs"
Installing "identity"
Skipping package "simple-json", using local path: "../purescript-simple-json"
Installing "control"
Installing "enums"
...
```


### Override a package in the package set with a remote one

Let's now say that we test that our fix works, and we are ready to Pull Request the fix.

So we push our fork and open the PR, but while we wait for the fix to land on the next
package-set release, we still want to use the fix in our production build.

In this case, we can just change the override to point to some commit of our fork, like this:


```haskell
let overrides =
    { simple-json =
          upstream.simple-json
       // { repo = "https://github.com/my-user/purescript-simple-json.git"
          , version = "701f3e44aafb1a6459281714858fadf2c4c2a977"
          }
    }
```

**Note**: you can use a "branch", a "tag" or a "commit hash" as a `version`.
Generally it's recommended that you avoid using branches, because if you push new
commits to a branch, `spago` won't pick them up unless you delete the `.spago` folder.


### Add a package to the package set

If a package is not in the upstream package-set, you can add it in a similar way,
by changing the `additions` record in the `packages.dhall` file.

E.g. if we want to add the `facebook` package:

```haskell
let additions =
  { facebook =
      mkPackage
        [ "console"
        , "aff"
        , "prelude"
        , "foreign"
        , "foreign-generic"
        , "errors"
        , "effect"
        ]
        "https://github.com/Unisay/purescript-facebook.git"
        "v0.3.0"
  }
```

The `mkPackage` function should be already included in your `packages.dhall`, and it will
expect as input a list of dependencies, the location of the package, and the tag you wish to use.

Of course this works also in the case of adding local packages. In this case you won't
care about the value of the "version" (since it won't be used), so you can put arbitrary
values in there.

And of course if the package you're adding has a `spago.dhall` file you can just import it
and pull the dependencies from there, instead of typing down the list of dependencies!

Example:

```haskell
let additions =
  { foobar =
      mkPackage
        (../foobar/spago.dhall).dependencies
        "../foobar"
        "local-fix-whatever"
  }
```


### `bower link`

See how to [add local packages](#add-a-package-to-the-package-set) or [override existing ones](#override-a-package-in-the-package-set-with-a-local-one)


### Verify that an addition/override doesn't break the package set

"But wait", you might say, "how do I know that my override doesn't break the package set?"

This is a fair question, and you can verify that your fix didn't break the rest of the
package-set by running the `verify` command.

E.g. if you patched the `foreign` package, and added it as a local package to your package-set,
you can check that you didn't break its dependents (also called "reverse dependencies")
by running:

```bash
$ spago verify foreign
```

Once you check that the packages you added verify correctly, we would of course very much love
if you could pull request it to the [Upstream package-set][package-sets] ❤️

If you decide so, you can read up on how to do it [here][package-sets-contributing].


### Automagically upgrade the package set

The version of the package-set you depend on is fixed in the `packages.dhall` file
(look for the `upstream` var).

You can upgrade to the latest version of the package-set with the `package-set-upgrade`
command, that will automatically find out the latest version, download it, and write
the new url and hashes in the `packages.dhall` file for you.

Running it would look something like this:

```bash
$ spago package-set-upgrade
Found the most recent tag for "purescript/package-sets": "psc-0.12.3-20190227"
Package-set upgraded to latest tag "psc-0.12.3-20190227"
Fetching the new one and generating hashes.. (this might take some time)
Done. Updating the local package-set file..
```

If you wish to detach from tags for your package-set, you can of course point it to a
specific commit. Just set your `upstream` to look something like this:

```haskell
let upstream =
      https://github.com/purescript/package-sets/blob/81354f2ea1ac9493eb05dfbd43adc6d183bc4ecd/src/packages.dhall
```


### Separate `devDependencies` or test dependencies

`spago` aims to support monorepos. This means that supporting "split" dependencies between tests
and apps or just for dev can be handled as a "monorepo situation".

So for example if you wish to separate dependencies for some `app` and `lib` you're working on,
you can handle it by having multiple `spago.dhall` config files for the lib and the executable.

E.g. let's say you have the following tree:

```
.
├── app
│   ├── spago.dhall
│   ├── src
│   │   └── Main.purs
│   └── test
│       └── Main.purs
├── lib
│   ├── spago.dhall
│   ├── src
│   │   └── Main.purs
│   └── test
│       └── Main.purs
└── packages.dhall
```

Then:
- the top level `packages.dhall` is standard and contains the link to the upstream and project-level overrides, etc
- `lib/spago.dhall` might look something like this:

```hs
{ name =
    "my-lib"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "prelude"
    ]
, packages =
    ../packages.dhall   -- Note: this refers to the top-level packages file
}
```

- `app/spago.dhall` might look something like this:

```hs
{ name =
    "my-app"
, dependencies =
    -- Note: the app does not include all the dependencies that the lib included
    [ "prelude"
    , "simple-json" -- Note: this dep was not used by the library, only the executable uses it
    , "my-lib"      -- Note: we add the library as dependency
    ]
, packages =
    -- We refer to the top-level packages file here too, so deps stay in sync
    -- and we also add the library as a local package
    (../packages.dhall) //
    { my-lib =
        { repo = "../my-lib"
        , version = ""
        , dependencies = (../my-lib/spago.dhall).dependencies
        }
    }
}
```

With this setup you're able to decouple dependencies in the library and in the executables.


### Bundle a project into a single JS file

For the cases when you wish to produce a single JS file from your PureScript project,
there are basically two ways to do that:

#### 1. `spago bundle-app`

This will produce a single, executable, dead-code-eliminated file:

```bash
# You can specify the main module and the target file, or these defaults will be used
$ spago bundle-app --main Main --to index.js
Bundle succeeded and output file to index.js

# We can then run it with node:
$ node .
```

#### 2. `spago bundle-module`

If you wish to produce a single, dead-code-eliminated JS module that you can `require` from
JavaScript:

```bash
# You can specify the main module and the target file, or these defaults will be used
$ spago bundle-module --main Main --to index.js
Bundling first...
Bundle succeeded and output file to index.js
Make module succeeded and output file to index.js

$ node -e "console.log(require('./index).main)"
[Function]
```

#### Skipping the Build Step

When running `spago bundle-app` and `spago bundle-module` the `build` step will also execute
since bundling depends on building first.

To skip this build you can add the `--no-build` flag.


### Make a project with PureScript + JavaScript

Take a look at [TodoMVC with react-basic + spago + parcel][todomvc] to have a starting point.

This generally consists of two separate build steps:

1. Build the project with `spago`:
  - this will compile your project to JS
  - you can use either `spago build` - which will create many files that you can require in your JS -
    or `spago bundle-module`, which will create only one Dead-Code-Eliminated file that you can require.
  - the tradeoff between the two is compile times vs bundle size: `bundle-module` will be more expensive
    to build, but will generally be smaller, while just requiring the artifacts from `build` is very fast
    to compile but might lead to a bigger bundle (you should benchmark this though)
2. Bundle the project with a JS bundler:
  - this will usually bundle everything in a single JS file, after resolving all the `require`s
    and including the JS dependencies
  - you'll usually have a `index.js` file in your project, that will include something like:
    ```js
    ..
    // So you require the PureScript file from js
    var PureScriptMain = require('./output/Main');
    ..
    // Then you can just call its functions from js
    PureScriptMain.somemethod();
    ```
  - the above example project uses `parcel`, but you can use `webpack`, `browserify`, etc.


### Generate documentation for my project

To build documentation for your project and its dependencies (i.e. a "project-local
[Pursuit][pursuit]"), you can use the `docs` command:
```bash
$ spago docs
```

This will generate all the documentation in the `./generated-docs` folder of your project.
You might then want to open the `index.html` file in there.

To build the documentation as Markdown instead of HTML, or to generate tags for your project,
you can pass a `format` flag:
```bash
$ spago docs --format ctags
```


### Publish my library

If you wish to develop a library with `spago` you can definitely do so, and use it to
manage and build your project, until you need to "publish" your library, where you'll need
to use `pulp`.

When you decide you want to publish your library for others to use, you should:
- run `spago bump-version --no-dry-run <BUMP>`. This will generate a `bower.json` in a new  commit in Git that is tagged with the version.
- run `pulp publish`. This will ensure the package is registered in Bower, push the version tag to Git and upload documentation to Pursuit.

This is because the PureScript ecosystem uses the Bower registry as a "unique names registry".
So in order to "publish" a package one needs to add it there, and eventually to [`package-sets`][package-sets].
Consequentially, package-sets requires (full instructions [here][package-sets-contributing])
that packages in it:
- are in the Bower registry
- use `spago bump-version` or `pulp version` (because this gives versions with `vX.Y.Z`)
- use `pulp publish` (so that's it's available on the Bower registry and on [Pursuit][pursuit])

All of this will be automated in future versions, removing the need for Pulp.

A library published in this way is [purescript-rave](https://github.com/reactormonk/purescript-rave).

### Use this together with `psc-package`

`spago` can help you setup your `psc-package` project to use the Dhall version of the package set.

We have two commands for it:
- **`psc-package-local-setup`**: this command creates a `packages.dhall` file in your project,
  that points to the most recent package set, and lets you override and add arbitrary packages.
  See the docs about this [here][package-sets].
- **`psc-package-insdhall`**: do the *Ins-Dhall-ation* of the local project setup: that is,
  generates a local package set for `psc-package` from your `packages.dhall`, and points your
  `psc-package.json` to it.

  Functionally this is equivalent to running:

  ```sh
  NAME='local'
  TARGET=.psc-package/$NAME/.set/packages.json
  mkdir -p .psc-package/$NAME/.set
  dhall-to-json --pretty <<< './packages.dhall' > $TARGET
  echo wrote packages.json to $TARGET
  ```

### Get all the licenses of my dependencies

For compliance reasons, you might need to fetch all the `LICENSE` files of your dependencies.

To do this you can exploit the `list-packages` command with its `--filter` flag.

E.g. if you want to print out all the `LICENSE` files of your direct dependencies:

```bash
#!/usr/bin/env bash

# Note: the `awk` part is to cut out only the package name
for dep in $(spago list-packages -f direct | awk '{print $1}')
do
  cat $(find ".spago/${dep}" -iname 'LICENSE')
done
```

### Know what `purs` commands are run under the hood

The `-v` flag will print out all the `purs` commands that `spago` invokes during its operations.


### Ignore or update the global cache

There is a global cache that `spago` uses to avoid re-downloading things - its
location will be printed if you call e.g. `spago install -v`.

It's possible to change the behaviour of the global cache with the `--global-cache` flag
that is accepted by many commands. You can either:
- skip the cache with `--global-cache=skip`: in this case the global cache will be ignored
  and the local project will re-download everything
- update the cache to the latest version with `--global-cache=update`: this might be useful
  if you want to globally cache a tag or commit that is newer than 24h - the time `spago` will
  wait before updating its metadata file about "which things are globally cacheable".

## Explanations

### Visual Overview: What happens when you do 'spago build'?

![spago-flowchart.svg](./diagrams/spago-flowchart.svg)

### Configuration file format

It's indeed useful to know what's the format (or more precisely, the [Dhall][dhall]
type) of the files that `spago` expects. Let's define them in Dhall:

```haskell
-- The basic building block is a Package:
let Package =
  { dependencies : List Text  -- the list of dependencies of the Package
  , repo = Text               -- the address of the git repo the Package is at
  , version = Text            -- git tag
  }

-- The type of `packages.dhall` is a Record from a PackageName to a Package
-- We're kind of stretching Dhall syntax here when defining this, but let's
-- say that its type is something like this:
let PackageSet =
  { console : Package
  , effect : Package
  ...                  -- and so on, for all the packages in the package-set
  }

-- The type of the `spago.dhall` configuration is then the following:
let Config =
  { name : Text               -- the name of our project
  , dependencies : List Text  -- the list of dependencies of our app
  , sources : List Text       -- the list of globs for the paths to always include in the build
  , packages : PackageSet     -- this is the type we just defined above
  }
```

### Why can't `spago` also install my npm dependencies?

A common scenario is that you'd like to use things like `react-basic`, or want to depend
on JS libraries like ThreeJS.
In any case, you end up depending on some NPM package.

And it would be really nice if `spago` would take care of installing all of these
dependencies, so we don't have to worry about running npm besides it, right?

While these scenarios are common, they are also really hard to support.
In fact, it might be that a certain NPM package in your transitive dependencies
would only support the browser, or only node. Should `spago` warn about that?
And if yes, where should we get all of this info?

Another big problem is that the JS backend is not the only backend around. For example,
PureScript has a [C backend][purec] and an [Erlang backend][purerl] among the others.

These backends are going to use different package managers for their native dependencies,
and while it's feasible for `spago` to support the backends themselves, supporting also
all the possible native package managers (and doing things like building package-sets for their
dependencies versions) is not a scalable approach (though we might do this in the future if
there's enough demand).

So this is the reason why if you or one of your dependencies need to depend on some "native"
packages, you should run the appropriate package-manager for that (e.g. npm).

For examples on how to do it, see next section.


### Why we don't resolve JS dependencies when bundling, and how to do it

`spago` only takes care of PureScript land. In particular, `bundle-module` will do the
most we can do on the PureScript side of things (dead code elimination), but will
leave the `require`s still in.

To fill them in you should use the proper js tool of the day, at the time of
writing [ParcelJS][parcel] looks like a good option.

If you wish to see an example of a project building with `spago` + `parcel`, a simple
starting point is the [TodoMVC app with `react-basic`][todomvc].
You can see in its `package.json` that a "production build" is just
`spago build && parcel build index.html`.

If you open its `index.js` you'll see that it does a `require('./output/Todo.App')`:
the files in `output` are generated by `spago build`, and then the `parcel` build resolves
all the `require`s and bundles all these js files in.

Though this is not the only way to include the built js - for a slimmer build or for importing
some PureScript component in another js build we might want to use the output of `bundle-module`.

For an example of this in a "production setting" you can take a look at [affresco][affresco].
It is a PureScript monorepo of React-based components and apps.

The gist of it is that the PureScript apps in the repo are built with `spago build`
(look in the `package.json` for it), but all the React components can be imported from
JS apps as well, given that proper modules are built out of the PS sources.

This is where `spago bundle-module` is used: the `build-purs.rb` builds a bundle out of every
single React component in each component's folder - e.g. let's say we `bundle-module` from
the `ksf-login` component and output it in the `index.js` of the component's folder; we can
then `yarn install` the single component (note it contains a `package.json`), and require it
as a separate npm package with `require('@affresco/ksf-login')`.

### How does the "global cache" work?

Every time `spago` will need to "install dependencies" it will:
- check if the package is local to the filesystem: if it is then it will skip it as we can just
  point to the files
- check if the ref is already in the global cache. If it is, it will just copy it
  to the project-local cache
- download [a metadata file from the `package-sets-metadata`][package-sets-metadata-file] repo
  if missing from the global cache or older 24 hours.
  
  This file contains the list of *tags* and *commits* for every package currently in the package
  set, updated hourly.
- check if the tag or commit of the package we need to download is in this cached index,
  and if it is then this means we can "globally cache" that version - this is because commit
  hashes are immutable, and tags are "immutable enough"
- if a version is deemed to be "globally cacheable" then a tarball of that ref is downloaded
  from GitHub and copied to both the global and the local cache
- otherwise, the repo is just cloned to the local cache

Note: a question that might come up while reading the above might be "why not just hit GitHub 
to check commits and tags for every repo while installing?"

The problem is that GitHub limits token-less API requests to 50 per hour, so any
decently-sized installation will fail to get all the "cacheable" items, making the 
global cache kind of useless. So we are just caching all of that info for everyone here.


## Troubleshooting

#### I added a git repo URL to my overrides, but `spago` thinks it's a local path 🤔

This might happen if you copy the "git" URL from a GitHub repo and try adding it as a repo URL
in your package set.

However, `spago` requires URLs to conform to [RFC 3986](https://tools.ietf.org/html/rfc3986),
which something like `git@foo.com:bar/baz.git` doesn't conform to.

To have the above repo location accepted you should rewrite it like this:
```
ssh://git@foo.com/bar/baz.git
```


#### My `install` command is failing with some errors about "too many open files"

This might happen because the limit of "open files per process" is too low in your OS - as
`spago` will try to fetch all dependencies in parallel, and this requires lots of file operations.

You can limit the number of concurrent operations with the `-j` flag, e.g.:

```
$ spago install -j 10
```

To get a ballpark value for the `j` flag you can take the result of the `ulimit -n` command
(which gives you the current limit), and divide it by four.


#### Package set caching problems

If you encounter any issues with the hashes for the package-set (e.g. the hash is not deemed
correct by `spago`), then you can have the hashes recomputed by running the `freeze` command:

```bash
$ spago freeze
```

However, this is a pretty rare situation and in principle it should not happen, and when
it happens it might not be secure to run the above command.

To understand all the implications of this I'd invite you to read about
[the safety guarantees][dhall-hash-safety] that Dhall offers.


#### I added a new package to the `packages.dhall`, but `spago` is not installing it. Why?

Adding a package to the package-set just includes it in the set of possible packages you
can depend on. However, if you wish `spago` to install it you should then add it to
the `dependencies` list in your `spago.dhall`.

## Internals

See [this document](./INTERNALS.md)

[pulp]: https://github.com/purescript-contrib/pulp
[purp]: https://github.com/justinwoo/purp
[dhall]: https://github.com/dhall-lang/dhall-lang
[cargo]: https://github.com/rust-lang/cargo
[stack]: https://github.com/commercialhaskell/stack
[purec]: https://github.com/pure-c/purec
[parcel]: https://parceljs.org
[purerl]: https://github.com/purerl/purescript
[pursuit]: https://pursuit.purescript.org/
[todomvc]: https://github.com/f-f/purescript-react-basic-todomvc
[affresco]: https://github.com/KSF-Media/affresco/tree/4b430b48059701a544dfb65b2ade07ef9f36328a
[spago-npm]: https://www.npmjs.com/package/spago
[new-issue]: https://github.com/spacchetti/spago/issues/new
[spago-nix]: https://github.com/justinwoo/easy-purescript-nix/blob/master/spago.nix
[purescript]: https://github.com/purescript/purescript
[psc-package]: https://github.com/purescript/psc-package
[contributing]: CONTRIBUTING.md
[package-sets]: https://github.com/purescript/package-sets
[travis-spago]: https://travis-ci.com/spacchetti/spago
[spago-issues]: https://github.com/spacchetti/spago/issues
[spacchettibotti]: https://github.com/spacchettibotti
[dhall-hash-safety]: https://github.com/dhall-lang/dhall-lang/wiki/Safety-guarantees#code-injection
[windows-issue-yarn]: https://github.com/spacchetti/spago/issues/187
[spago-latest-release]: https://github.com/spacchetti/spago/releases/latest
[ubuntu-issue-netbase]: https://github.com/spacchetti/spago/issues/196
[ubuntu-issue-libtinfo]: https://github.com/spacchetti/spago/issues/104#issue-408423391
[package-sets-metadata]: https://github.com/spacchetti/package-sets-metadata
[package-sets-contributing]: https://github.com/purescript/package-sets/blob/master/CONTRIBUTING.md
[package-sets-metadata-file]: https://github.com/spacchetti/package-sets-metadata/blob/master/metadataV1.json
