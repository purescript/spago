# spago

[![npm](https://img.shields.io/npm/v/spago.svg)][spago-npm]
[![Latest release](https://img.shields.io/github/v/release/purescript/spago.svg)](https://github.com/purescript/spago/releases)
[![build](https://github.com/purescript/spago/actions/workflows/build.yml/badge.svg)](https://github.com/purescript/spago/actions/workflows/build.yml)
[![Maintainer: f-f](https://img.shields.io/badge/maintainer-f%2d-f-teal.svg)](http://github.com/f-f)

*(IPA: /ˈspaɡo/)*

PureScript package manager and build tool.

<p align="center">
<img src="./logo.png" height="500px" alt="Spago logo - a 3d box containing a blob of spaghetti">
</p>

## Installation

> [!IMPORTANT]\
> This documentation concerns the new PureScript rewrite of Spago. If you are looking for the Haskell codebase (versions up to 0.21.x), please head over to the [spago-legacy] repo.

> [!WARNING]\
> This new Spago is still in alpha, so while most of it works well, there will be some rough edges here and there. Please report them if you find any!

The recommended installation method for Windows, Linux and macOS is `npm` (see the latest releases on npm
  [here][spago-npm]):

```
npm install -g spago@next
```

Other installation methods available:
- With Nix, using [purescript-overlay]

**General notes:**
- The assumption is that you already installed the [PureScript compiler][purescript].
  If not, get it with `npm install -g purescript`, or the recommended method for your OS.
- You might have issues with `npm` and Docker (e.g. getting the message "Downloading the spago binary failed.." etc)
  You have two options:
  - either **do not run npm as root**, because it doesn't work well with binaries. Use it as a nonprivileged user.
  - or use `--unsafe-perm`: `npm install -g --unsafe-perm spago@next`


## Super quick tutorial

Let's set up a new project!

```console
$ mkdir purescript-pasta
$ cd purescript-pasta
$ spago init
```

This last command will create a few files:

```
.
├── spago.yaml
├── src
│   └── Main.purs
└── test
    └── Test
        └── Main.purs
```

If you have a look at the `spago.yaml` file, you'll see that it contains two sections:
- [the `workspace` section](#the-workspace), which details the configuration for the _dependencies_ of the project as a whole (which can be a monorepo, and contain more than one package), and other general configuration settings.
  In this sample project, the only configuration needed is the [package set](#whats-a-package-set) version from which all the dependencies will be chosen. See [here](#querying-package-sets) for more info about how to query the package sets.
- [the `package` section](#whats-a-package), that is about the configuration of the package at hand, such as its name, dependencies, and so on.

For more info about all the various configuration settings, visit the section about [the configuration format](#the-configuration-file).

To build and run your project, use:

```console
$ spago run
```

This will:
- download and compile the necessary dependencies (equivalent to `spago install`)
- compile this sample project in the `output/` directory (equivalent to `spago build`).\
  You can take a look at the content of `output/Main/index.js` to see what kind of JavaScript has been generated from your new `Main.purs` file
- run the generated JS, which is roughly equivalent to running
  ```console
  $ node -e "import('./output/Main/index').then(m => m.main())"
  ```
  The above code imports the JS file you just looked at, and runs its `main` with Node.

You can also bundle the project in a single file with an entry point, so it can be run directly (useful for CLI apps):

```console
$ spago bundle --bundle-type app --platform node
$ node .
```

Great! If you read unitl here you should be set to go write some PureScript without worrying too much about the build 😊

Where to go from here? There are a few places you should check out:
- see [the "How to achieve X"](#how-do-i) section for practical advice without too much explanation
- see instead the [Concepts and Explanations](#concepts-and-explanations) section for more in-depth explanations about the concepts that power Spago, such as [package sets](#whats-a-package-set), or [the Workspace](#the-workspace).

## Table of contents

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Design goals and reasons](#design-goals-and-reasons)
- [Developing and contributing](#developing-and-contributing)
- [How do I...](#how-do-i)
  - [Migrate from `spago.dhall` to `spago.yaml`](#migrate-from-spagodhall-to-spagoyaml)
  - [Migrate from `bower`](#migrate-from-bower)
  - [See what commands and flags are supported](#see-what-commands-and-flags-are-supported)
  - [Setup a new project using a specific package set](#setup-a-new-project-using-a-specific-package-set)
  - [Setup a new project using the solver](#setup-a-new-project-using-the-solver)
  - [Install a direct dependency](#install-a-direct-dependency)
  - [Download my dependencies locally](#download-my-dependencies-locally)
  - [Build and run my project](#build-and-run-my-project)
  - [Test my project](#test-my-project)
  - [Run a repl](#run-a-repl)
  - [Run a standalone PureScript file as a script](#run-a-standalone-purescript-file-as-a-script)
  - [List available packages](#list-available-packages)
  - [Install all the packages in the set](#install-all-the-packages-in-the-set)
  - [Override a package in the package set with a local one](#override-a-package-in-the-package-set-with-a-local-one)
  - [Override a package in the package set with a remote one](#override-a-package-in-the-package-set-with-a-remote-one)
  - [Add a package to the package set](#add-a-package-to-the-package-set)
  - [Querying package sets](#querying-package-sets)
  - [Upgrading packages and the package set](#upgrading-packages-and-the-package-set)
  - [Custom package sets](#custom-package-sets)
  - [Monorepo support](#monorepo-support)
  - [Polyrepo support](#polyrepo-support)
  - [Test dependencies](#test-dependencies)
  - [Bundle a project into a single JS file](#bundle-a-project-into-a-single-js-file)
  - [Enable source maps](#enable-source-maps)
    - [Node](#node)
    - [Browsers](#browsers)
  - [Skipping the "build" step](#skipping-the-build-step)
  - [Generated build info/metadata](#generated-build-infometadata)
  - [Generate documentation for my project](#generate-documentation-for-my-project)
  - [Alternate backends](#alternate-backends)
  - [Publish my library](#publish-my-library)
  - [Know which `purs` commands are run under the hood](#know-which-purs-commands-are-run-under-the-hood)
  - [Install autocompletions for `bash`](#install-autocompletions-for-bash)
  - [Install autocompletions for `zsh`](#install-autocompletions-for-zsh)
- [Concepts and explanations](#concepts-and-explanations)
  - [What's a "package"?](#whats-a-package)
  - [What's a "package set"?](#whats-a-package-set)
  - [The workspace](#the-workspace)
  - [The configuration file](#the-configuration-file)
  - [The lock file](#the-lock-file)
- [FAQ](#faq)
  - [Why can't `spago` also install my npm dependencies?](#why-cant-spago-also-install-my-npm-dependencies)
- [Differences from legacy spago](#differences-from-legacy-spago)
  - [Watch mode](#watch-mode)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


## Design goals and reasons

Our main design goals are:
- **Great UX**: a good build system just does what's most expected and gets out of the way so you can focus on actually thinking about the software itself, instead of spending your time configuring the build.
- **Minimal dependencies**: users should not be expected to install a myriad of tools on their system to support various workflows. Spago only expects `git` and `purs` to be installed.
- **Reproducible builds**: we exploit [package sets](#whats-a-package-set) and [lock files](#the-lock-file) to make your build reproducible, so that if your project builds today it will also build tomorrow and every day after that.

Some tools that inspired `spago` are: [Rust's Cargo][cargo], [Haskell's Stack][stack],
[`psc-package`][psc-package], [`pulp`][pulp] and [`bazel`][bazel].

## Developing and contributing

We'd love your help, and welcome PRs and contributions!

Some ideas for getting started:
- [Build and run `spago`](CONTRIBUTING.md#developing-spago)
- [Help us fix bugs and build features](https://github.com/purescript/spago/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+label%3A%22defect%22)
- Help us improve our documentation
- Help us [log bugs and open issues][new-issue]

For more details see the [`CONTRIBUTING.md`][contributing]


## How do I...

This section contains a collection of mini-recipes you might want to follow in order to get things done with Spago.

### Migrate from `spago.dhall` to `spago.yaml`

You'll need to use [spago-legacy] for this:

```bash
# Install spago-legacy
npm install spago@0.21.0

# You can then create a `spago.yaml` file with `migrate`
spago migrate

# Ready to remove the dhall files and move to the new spago
npm install spago@next
rm spago.dhall packages.dhall
```

Some packages might not be found or have the wrong version, in which case
you'll have to carefully:
- try to run `spago install some-package` for packages found in the package set (see [how to query the set](#querying-package-sets))
- [add the packages that are missing from the set](#add-a-package-to-the-package-set)

In **all** cases, you'll want to switch to the new Registry package sets, so replace something like this:
```yaml
workspace:
  package_set:
    url: https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.10-20230919/packages.json
```

...with this:
```yaml
workspace:
  package_set:
    registry: 41.2.0
```

This is because the legacy package set format - while being supported - is pointing at git repositories, so Spago will fetch them using git, which can get quite slow and error-prone.

The new package sets are instead pointing at the Registry, and can fetch compressed archives from our CDN, which is much faster and more reliable.

To figure out what package set you're supposed to be using, see the section about [querying package sets](#querying-package-sets).

You might also want to check out the section about [differences from legacy spago](#differences-from-legacy-spago).

### Migrate from `bower`

Same as above, but with an additional `spago init` command just after you install [spago-legacy], so that the `bower.json` file is converted into a `spago.dhall` file.

### See what commands and flags are supported

For an overview of the available commands, run:

```console
$ spago --help
```

You will see several subcommands (e.g. `build`, `test`); you can ask for help
about them by invoking the command with `--help`, e.g.:

```console
$ spago build --help
```

This will give a detailed view of the command, and list any flags you can use with that command.

### Setup a new project using a specific package set

Since `spago init` does not necessarily use the latest package set. Fortunately, you can specify which package set to use via the `--package-set` flag:

```console
$ spago init --package-set 41.2.0
```

See [here](#querying-package-sets) for how to ask Spago which sets are available for use.

### Setup a new project using the solver

Package sets are the default experience to ensure that you always get a buildable project out of the box, but one does not necessarily have to use them.

If you'd like to set up a project that uses the Registry solver to figure out a build plan, you can use:

```console
$ spago init --use-solver
```

When using the solver (and when [publishing a package](#publish-my-library)), it's important to specify the version bounds for your dependencies, so that the solver can figure out a build plan.

You can ask Spago to come up with a good set of bounds for you by running:

```console
$ spago install --ensure-ranges
```

### Install a direct dependency

To add a dependency to your project you can run:

```console
# E.g. installing Halogen
$ spago install halogen

# This also supports multiple packages
$ spago install foreign aff
```

If you are using the Registry solver then the package must be available in the Registry, while if you
are using package sets it needs to be contained in the set. See [here](#add-a-package-to-the-package-set) to know more about adding more packages to the local set.

### Download my dependencies locally

```console
$ spago install
```

This will download and compile all the transitive dependencies of your project (i.e. the direct dependencies,
i.e. the ones listed in the `dependencies` key of `spago.yaml`, plus all their dependencies,
recursively) to the local `.spago` folder.

However, running this directly is usually **not necessary**, as all commands that need the dependencies
to be installed will run this for you.

Running `spago fetch` is equivalent, but skips the compilation step.

### Build and run my project

You can build the project and its dependencies by running:

```console
$ spago build
```

This is mostly just a thin layer above the PureScript compiler command `purs compile`.

*Note*: by default the `build` command will try to install any dependencies that haven't been
fetched yet - if you wish to disable this behaviour, you can pass the `--no-install` flag.

The build will produce very many JavaScript files in the `output/` folder. These
are ES modules, and you can just `import` them e.g. on Node.

> [!NOTE]\
> The wrapper on the compiler is so thin that you can pass options to `purs`.
> E.g. if you wish to ask `purs` to emit errors in JSON format, you can run
> ```console
> $ spago build --purs-args "--json-errors"
> ```
> However, some `purs` flags are covered by Spago ones, e.g. to change the location of the `output` folder:
> ```console
> $ spago build --output myOutput
> ```

To run a command before a build you can use the `--before` flag, eg to post a notification that a build has started:

```console
$ spago build --before "notify-send 'Building'"
```

To run a command after the build, use `--then` for successful builds, or `--else` for unsuccessful builds:

```console
$ spago build --then "notify-send 'Built successfully'" --else "notify-send 'Build failed'"
```

Multiple commands are possible - they will be run in the order specified:

```console
$ spago build --before clear --before "notify-send 'Building'"
```

If you want to run the program (akin to `pulp run`), just use `run`. The commands below are verbose until these values are defined in the configuration file:
```console
$ spago run -p package-name -m Module.Containing.Main

# We can pass arguments through to `purs compile`
$ spago run -p package-name  -m Module.Containing.Main --purs-args "--verbose-errors"

# We can pass arguments through to the program being run
$ spago run -p package-name  -m Module.Containing.Main -- arg1 arg2
```

Oof! That's a lot to run a command. If you configure these parameters in the package's configuration file's `package.run` section, you don't have to supply them at the command line.
See [here](#the-configuration-file) for more info about this, but it allows us to instead write.

```console
# module defined
spago run -p package-name
spago run -p --purs-args "--verbose-errors"

# Args passed to program defined
$ spago run -p package-name
```

Lastly, if you only have a single package defined in the workspace with these parameters defined in the config file, you can just run

```console
spago run
```

### Test my project

You can also test your project with `spago`:

```console
# Test.Main is the default here, but you can override it as usual
$ spago test --main Test.Main
Build succeeded.
You should add some tests.
Tests succeeded.
```

As with the `run` command, it's possible to configure the tests using the `spago.yaml` - most importantly to separate test dependencies from the dependencies of your application/library.

Please see [the section about the configuration format](#the-configuration-file) for more info, but in the meantime note that it's possible to install test dependencies by running:

```console
$ spago install --test-deps spec
```

### Run a repl

You can start a repl with the following command:

```console
$ spago repl
```

### Run a standalone PureScript file as a script

You can run a standalone PureScript file as a script via `spago script`.
Note: The module name must be `Main`, and it must export a function `main :: Effect Unit`.

By default, the following dependencies are installed: `effect`, `console`, `prelude`.

You can run a script via the following, optionally specifying a package set to use, and additional dependencies to pull from there:

```console
$ spago script --package-set 41.2.0 -d node-fs path/to/script.purs
```

### List available packages

It is sometimes useful to know which packages are contained in our package set
(e.g. to see which version we're using, or to search for packages).

You can get a complete list of the packages provided by your `workspace` (together with their versions, locations, and license) by running:

```console
$ spago ls packages
```

By using the `ls deps` command instead you can restrict the list to direct or transitive dependencies:

```console
# Direct dependencies, i.e. only the ones listed in spago.dhall
$ spago ls deps

# Transitive dependencies, i.e. all the dependencies of your dependencies
$ spago ls deps --transitive
```

You can provide the `--json` flag for a more machine-friendly output.

### Install all the packages in the set

There might be cases where you'd like your project to depend on all the packages
that are contained in the package set (this is sometimes called
["acme build"][acme]).

If you have [`jq`][jq] installed, you can accomplish this in relatively few characters:

```console
$ spago ls packages --json | jq -r 'keys[]' | xargs spago install
```

### Override a package in the package set with a local one

Let's say I'm a user of the popular `aff` package. Now, let's say I stumble upon a bug
in there, but thankfully I figure how to fix it. So I clone it locally and add my fix.

Now if I want to test this version in my current project, how can I tell `spago` to do it?

There's a section of the `spago.yaml` file just for that, called `extra_packages`.

In this case we override the package with its local copy, which should have a `spago.yaml` - our `workspace` will look something like this:

```yaml
workspace:
    registry: 41.2.0
  extra_packages:
    aff:
      path: ../my-purescript-aff
```

Now if we run `spago ls packages`, we'll see that it is now included as a local package:

```console
$ spago ls packages
+----------------------------------+------------------------------------------+------------------------------------------------+
| Package                          | Version                                  | Location                                       |
+----------------------------------+------------------------------------------+------------------------------------------------+
| abc-parser                       | 2.0.0                                    | -                                              |
| ace                              | 9.1.0                                    | -                                              |
| aff                              | local                                    | ../my-purescript-aff                           |
| aff-bus                          | 6.0.0                                    | -                                              |
| aff-coroutines                   | 9.0.0                                    | -                                              |
| aff-promise                      | 4.0.0                                    | -                                              |
...
```

### Override a package in the package set with a remote one

Let's now say that we test that our fix from above works, and we are ready to Pull Request the fix.

So we push our fork and open the PR, but we want to already use the fix in our build, while we wait for it to land upstream and then on the next package set.

In this case, we can just change the override to point to some commit of our fork, like this:

```yaml
workspace:
    registry: 41.2.0
  extra_packages:
    aff:
      git: https://github.com/my-user/purescript-aff.git
      ref: aaa0aca7a77af368caa221a2a06d6be2079d32da
```

> [!WARNING]\
> You can use a "branch", a "tag" or a "commit hash" as a `version`.
> It's **strongly** recommended to avoid using branches, because if you push new commits to a branch, `spago` won't pick them up unless you delete the `.spago/packages/aff/your-branch` folder.

### Add a package to the package set

> [!IMPORTANT]\
> You still need to `spago install my-new-package` after adding it to the package set, or Spago will not know that you want to use it as a dependency!

If a package is not in the upstream package set, you can add it exactly in the same way, by adding it to `extra_packages`.

E.g. if we want to add the `facebook` package:

```yaml
workspace:
    registry: 41.2.0
  extra_packages:
    facebook:
      git: https://github.com/Unisay/purescript-facebook.git
      ref: v0.3.0 # branch, tag, or commit hash
```

> [!NOTE]\
> If the upstream library that you are adding has a `spago.yaml` file, then Spago will just pick up the dependencies from there.
> If that's not the case, then you'll have the provide the dependencies yourself, adding a `dependencies` field.


As you might expect, this works also in the case of adding local packages:

```yaml
workspace:
    registry: 41.2.0
  extra_packages:
    facebook:
      path: ../my-purescript-facebook
```

### Querying package sets

Since the versioning scheme for package sets does not tell anything about the compiler version or when they were published, you might want
to have a look at the list of all the available ones. You can do that with:

```console
$ spago registry package-sets
```

This will print a list of all the package sets ever releases, which could be overwhelming, as you'd likely only be interested in the latest one.

This is how you would ask for the latest package sets for each compiler version:

```console
$ spago registry package-sets --latest
+---------+------------+----------+
| VERSION | DATE       | COMPILER |
+---------+------------+----------+
| 10.0.0  | 2023-01-05 | 0.15.4   |
| 20.0.3  | 2023-04-08 | 0.15.7   |
| 27.2.0  | 2023-06-17 | 0.15.8   |
| 29.1.0  | 2023-07-18 | 0.15.9   |
| 42.1.0  | 2023-09-26 | 0.15.10  |
+---------+------------+----------+
```

### Upgrading packages and the package set

If your project is using the Registry solver (i.e. no package set and only version bounds), then running `spago upgrade`
will try to put together a new build plan with the latest package versions published on the Registry, given that they are still compatible with your current compiler.

If instead you are using package sets, then `spago upgrade` will bump your package set version to the latest package set available for your compiler version.

You can pass the `--package-set` flag if you'd rather upgrade to a specific package set version.
You can of course just edit the `workspace.package_set` field in the `spago.yaml` file.

### Custom package sets

Spago supports fetching custom package sets from URLs and paths, so you can build your own package set if you'd like - this is useful for example if you want to put together a custom package set for your company, or if you are using an [alternate backend](#alternate-backends).

Spago will be happy to use a package set from a local path:

```yaml
workspace:
  package_set:
    path: ../my-custom-package-set.json
```

Otherwise you can point Spago to any URL on the internet:

```yaml
workspace:
  package_set:
    url: https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.7-20230207/packages.json
```

...and it will try to fetch the content, parse it as JSON and conform it to one of the possible package set schemas.

The first one is what Spago calls a `RemotePackageSet`, which contains some metadata, and a map of packages in the shapes (2), (3) and (4) described for `extra_packages` in the [configuration format section](#the-configuration-file).

This package set could look something like this:

```js
{
  compiler: "0.15.10",
  version: "0.0.1",
  packages: {
    "some-registry-package": "1.0.2",
    "some-package-from-git-with-a-spago-yaml": {
      "git": "https://github.com/purescript/registry-dev.git",
      "ref": "68dddd9351f256980454bc2c1d0aea20e4d53fa9"
    },
    "legacy-package-style": {
      "repo": "https://github.com/purescript/purescript-prelude.git",
      "version": "v6.0.1",
      "dependencies": ["prelude", "effect", "console"]
    }
  }
}
```

The second format possible is what Spago calls a `LegacyPackageSet`, and it's simply a map from package names to the location of the package, described as the (4) option for how to specify `extra_packages` in the [configuration format section](#the-configuration-file).

Something like this:
```js
{
  "legacy-package-style": {
    "repo": "https://github.com/purescript/purescript-prelude.git",
    "version": "v6.0.1",
    "dependencies": ["prelude", "effect", "console"]
  }
  "metadata": {
    "repo": "https://github.com/purescript/metadata.git",
    "version": "v0.15.10",
    "dependencies": []
  }
}
```

This is supported to allow for just using legacy package sets, and be able to automatically migrate `spago.dhall` files to `spago.yaml` files.

It is not recommended to craft your own package set in the legacy format - please use the `RemotePackageSet` format instead - but if you do just be aware that you'll need to include a package called `metadata` that has a version that matches the compiler version that the set is supposed to support.

### Monorepo support

Spago supports ["monorepos"][luu-monorepo] (see [here][monorepo-tools] as well for more monorepo goodness), allowing you to split a pile of code
into different "compilation units" that might have different dependencies, deliverables, etc, but still compile together.

The vast majority of Spago projects will contain only one package, defined in the `package` section of the same `spago.yaml` that contains its `workspace`.
It is however possible to define multiple packages in the same repository!

The basic rules are:
- [a package](#whats-a-package) is defined by a `spago.yaml` file containing a `package` section.
- there can be only one `workspace` section in the whole repository, which defines the "root" of the current [Spago Workspace](#the-workspace). This defines your package set/build plan.
- Spago will autodetect all the packages inside the workspace
- ...except for `spago.yaml` files with a `workspace` section, which will be ignored (together with their subfolders, as they establish a the boundary of another "workspace")

For more info about the concept of Spago Workspaces, see [the dedicated section](#the-workspace).

Since this might sound a little abstract, let's try to picture the case where you might want to have the packages `lib1`, `lib2` and `app1`.

Then your file tree might look like this:

```
.
├── app1
│   ├── spago.yaml
│   ├── src
│   │   └── Main.purs
│   └── test
│       └── Main.purs
├── lib1
│   ├── spago.yaml
│   └── src
│       └── Main.purs
├── lib2
│   ├── spago.yaml
│   ├── src
│   │   └── Main.purs
│   └── test
│       └── Main.purs
└── spago.yaml
```

Where:
- the top level `spago.yaml` could look like this:

  ```yaml
  workspace:
    package_set:
      registry: 41.2.0
  ```

- and the `lib1/spago.yaml` would look something like this:

  ```yaml
  package:
    name: lib1
    dependencies:
    - effect
    - console
    - prelude
  ```

- then, assuming `lib2` depends on `lib1`, `lib2/spago.yaml` might look like this:

  ```yaml
  package:
    name: lib2
    dependencies:
    - effect
    - console
    - prelude
    - lib1 # <------ Note the dependency here
    tests:
      main: Test.Lib2.Main
      dependencies:
      - spec
  ```

- and then `app1/spago.yaml` would look something like this:

  ```yaml
  package:
    name: app1
    # Note that the app does not include all the dependencies that the lib included
    dependencies:
    - prelude
    - aff # This dep was not used by the library
    - lib2 # And we have `lib2` as a dependency
  ```

Given this setup, Spago will figure out that there are three separate packages in the repository.

You can _select_ a package to perform operations on it by using the `--package` flag, e.g. the follow will install the `maybe` package in the `lib1/spago.yaml`:

```console
spago install -p lib1 maybe
```

The `--package` flag is also available for many more commands, such as `build`, `run`, `test`, `bundle` and so on.

An important property of this "monorepo setup" is that the `output` folder will be shared between all the packages: they will share the same build package set (or build plan when using the solver) and they will be all build together.

### Polyrepo support

There might be cases where you want to have multiple loosely-connected codebases in the same repository that do _not_ necessarily build together all the time. This is sometimes called [a "polyrepo"][monorepo-tools].

One such example of this could be a project that has a frontend and a backend, and they are both written in PureScript, but run on different backends: the frontend runs in the browser (so in JavaScript), and the backend runs on Erlang.

Let's say you might also want to share some code between the two (that was the point of using the same language, no?), so you might have a `common` package that is used by both.

You can achieve all of this with Spago, by having multiple workspaces - let's try to visualise this.

The file tree might look like this:

```
.
├── client
│   ├── spago.yaml
│   ├── src
│   │   └── Main.purs
│   └── test
│       └── Test
│           └── Main.purs
├── common
│   ├── spago.yaml
│   ├── src
│   │   └── Main.purs
│   └── test
│       └── Test
│           └── Main.purs
└── server
    ├── spago.yaml
    ├── src
    │   └── Main.purs
    └── test
        └── Test
            └── Main.purs
```

Where the `common/spago.yaml` is just a package with no workspace defined, as it's going to support both the JS and the Erlang backend:
```yaml
package:
  name: common
  dependencies:
  - effect
  - console
  - prelude
```

Then the `client/spago.yaml` might look like this:
```yaml
workspace:
  package_set:
    registry: 41.2.0
  extra_packages:
    common:
      path: ../common
package:
  name: client
  dependencies:
  - prelude
  - common
  - halogen
```

And the `server/spago.yaml` might look like this:
```yaml
workspace:
  package_set:
    url: https://raw.githubusercontent.com/purerl/package-sets/erl-0.15.3-20220629/packages.json
  backend:
    cmd: purerl
  extra_packages:
    common:
      path: ../common
package:
  name: server
  dependencies:
  - prelude
  - common
  - erl-process
```

This all means that:
- there is a [Spago Workspace](#the-workspace) in the `client` folder, another one in the `server` folder, but none in the `common` folder
- the `common` package is shared between the two workspaces, note that it's included as a local package in both
- the `client` workspace uses the default JS package set, and the `server` workspace uses a Purerl package set
- to use each workspace you would need to `cd` into its folder, so that Spago can detect the workspace

### Test dependencies

Like this:

```yaml
package:
  name: mypackage
  dependencies:
  - effect
  - console
  - prelude
  tests:
    main: Test.Main
    dependencies:
    - spec
```

You can add more with `spago install --test-deps some-new-package`.

### Bundle a project into a single JS file

Use `spago bundle`.

This is a good-defaults wrapper into `esbuild`, and it's meant to be used for bundling small projects. Once your project grows in size, you might want to look into configuring `esbuild` (or `parcel`, or `webpack`) directly.

See the [`esbuild` getting started][install-esbuild] for installation instructions.

This command supports a few options, and the most important ones are:
- the `--bundle-type` flag, which can be either `app` or `module`
- the `--platform` flag, which can be either `browser` or `node`

See the help message for more flags, and [the configuration format section](#the-configuration-file) for how to configure these options in the `spago.yaml` file.

When bundling an `app`, the output will be a single executable file:

```console
$ spago bundle --to index.js --bundle-type app --platform node

# It is then possible to run it with node:
$ node index.js
```

> [!NOTE]\
> Spago will bundle your project in the [esbuild bundle format](https://esbuild.github.io/api/#format) [IIFE](https://esbuild.github.io/api/#format-iife).

When bundling a `module` instead, the output will be a single JS module that you can `import` from JavaScript:

```console
# You can specify the main module and the target file, or these defaults will be used
$ spago bundle --bundle-type module --main Main --outfile index.js
```

Can now import it in your Node project:
```console
$ node -e "import('./index.js').then(m => console.log(m.main))"
[Function]
```

### Enable source maps

When bundling, you can include `--source-maps` to generate a final source map for your bundle.

Example:
```console
spago bundle -p my-project --source-maps --minify --outfile=bundle.js
```
will generate a minified bundle: `bundle.js`, and a source map: `bundle.js.map`.

#### Node
If your target platform is node, then you need to ensure your node version is >= 12.2.0 and [enable source maps](https://nodejs.org/dist/latest-v20.x/docs/api/cli.html#--enable-source-maps
) when executing your script:
```console
spago bundle -p my-project --platform node --source-maps --minify --outfile=bundle.js
node --enable-source-maps bundle.js
```

#### Browsers
If you are targeting browsers, then you will need to ensure your server is configured to serve the source map from the same directory as your bundle.

So for example if your server is configured to serve files from `public/`, you might run:
```console
spago bundle -p my-project --platform browser --source-maps --minify --outfile=public/bundle.js
```

### Skipping the "build" step

When running `spago bundle`, Spago will first try to `build` your project, since bundling requires the project to be compiled first.

If you already compiled your project and want to skip this step you can pass the `--no-build` flag.

### Generated build info/metadata

Spago will include some metadata in the build, such as the version of the compiler used, the version of Spago, and the versions of the package itself.

This is so that you can access all these things from your application, e.g. to power a `--version` command in your CLI app.

This info will be available in the `Spago.Generated.BuildInfo` module, which you can import in your project.

The file itself is stored in the `.spago` folder if you'd like to have a look at it.

### Generate documentation for my project

To build documentation for your project and its dependencies (i.e. a "project-local
[Pursuit][pursuit]"), you can use the `docs` command:
```console
$ spago docs
```

This will generate all the documentation in the `./generated-docs` folder of your project.
You might then want to open the `index.html` file in there.

If you wish for the documentation to be opened in browser when generated, you can pass an `open` flag:
```console
$ spago docs --open
```

To build the documentation as Markdown instead of HTML, or to generate tags for your project,
you can pass a `format` flag:
```console
$ spago docs --format ctags
```

### Alternate backends

Spago supports compiling with alternate purescript backends, such as [purerl].

To use an alternate backend, include the `workspace.backend` section in your workspace's `spago.yaml`. See the [configuration format section](#the-configuration-file) for more info.

### Publish my library

To publish your library to the [PureScript Registry][registry], you can run:

```console
$ spago publish
```

...and follow the instructions 🙂

### Know which `purs` commands are run under the hood

The `-v` flag will print out all the `purs` commands that `spago` invokes during its operations,
plus a lot of diagnostic info, so you might want to use it to troubleshoot weird behaviours and/or crashes.

### Install autocompletions for `bash`

You can just add this to your `.bashrc`:

```bash
source <(spago --bash-completion-script `which spago`)
```

or alternatively if you don't want to edit your `~/.bashrc`:

```bash
spago --bash-completion-script $(which spago) >> ~/.bash_completion
```

### Install autocompletions for `zsh`

Autocompletions for `zsh` need to be somewhere in the `fpath` - you can see the folders
included in your by running `echo $fpath`.

You can also make a new folder - e.g. `~/.my-completions` - and add it to the `fpath`
by just adding this to your `~/.zshrc`:

```bash
fpath=(~/.my-completions $fpath)
```

Then you can obtain the completion definition for zsh and put it in a file called
`_spago` [(yes it needs to be called like that)](https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org#telling-zsh-which-function-to-use-for-completing-a-command):

```bash
spago --zsh-completion-script $(which spago) > ~/.my-completions/_spago
```

Then, reload completions with:

```bash
compinit
```

*Note*: you might need to call this multiple times for it to work.


## Concepts and explanations

This section details some of the concepts that are useful to know when using Spago. You don't have to read through this all at once, it's meant to be a reference for when you need it.

### What's a "package"?

Spago considers a "package" any folder that contains:
- a `spago.yaml` file with a valid `package` section
- a `src` subfolder with PureScript source files

That's all there is to it! You can have many of these in your repository if you'd like, and build them all together. See [the monorepo section](#monorepo-support) for more details.

The above holds for "workspace packages", i.e. the packages for which you have the source locally, and inside your repository. These are the packages "in your project".

Packages on which your project depends on can come from a few different sources:
- [the Registry][registry]
- local packages - i.e. packages that are on your filesystem but external to your repository
- remote packages - i.e. packages that are not on your filesystem, but somewhere on the internet

The bulk of the packages in your build will come from the Registry (often via a package set), but you are able to add local and remote packages to your build as well, by adding them to the `workspace.extra_packages` section of your `spago.yaml` file.

See [here](#add-a-package-to-the-package-set) and [here](#the-configuration-file) for more info about how to add these "extra packages".

Packages have "dependencies", which are other packages that are required for them to build. These dependencies are listed in the `dependencies` field of the `package` section of the `spago.yaml` file. See [here](#the-configuration-file) for more info about the structure of a `package` configuration.

### What's a "package set"?

The most generic way of defining a "package set" is "a collection of package versions that are known to build together". The point of a package set is to provide a "stable" set of packages that you can use to build your project, without worrying about version conflicts.

In practice, it looks something like [this][sample-package-set]:
```json
{
  "version": "41.2.0",
  "compiler": "0.15.10",
  "published": "2023-09-15",
  "packages": {
    "abc-parser": "2.0.1",
    "ace": "9.1.0",
    "aff": "7.1.0",
    "aff-bus": "6.0.0",
    "aff-coroutines": "9.0.0",
    "aff-promise": "4.0.0",
    "aff-retry": "2.0.0",
    "affjax": "13.0.0",
    "affjax-node": "1.0.0",
    "affjax-web": "1.0.0",
    "ansi": "7.0.0",
    "argonaut": "9.0.0",
    ...
  }
}
```

The Registry maintains an "official" package set such as the above, which is used by default by Spago, and _only_ contains packages that are contained in the Registry.

Whenever anyone publishes a new package version to the Registry, the pipeline will try to build this package together with the existing set, and if the build succeeds then the new version will be added to this official set.

However, Spago also supports using custom package sets, which can contain packages that are not in the Registry, and can be used to override existing packages with local or remote versions. See [here](#custom-package-sets) for more info.

### The workspace

For any software project, it's usually possible to find a clear line between "the project" and "the dependencies of the project": we "own" our sources, while the dependencies only establish some sort of substrate over which our project lives and thrives.

Following this line of reasoning, Spago - taking inspiration from other tools such as [Bazel][bazel] - uses the concept of of a "workspace" to characterise the sum of all the project packages and their dependencies (including only "potential" ones).

A very succint introduction to this idea can be found [in Bazel's documentation][bazel-workspace]:
> A workspace is a directory tree on your filesystem that contains the source files for the software you want to build.\
> Each workspace has a text file named `WORKSPACE` which may be empty, or may contain references to external dependencies required to build the outputs.\
> Directories containing a file called `WORKSPACE` are considered the root of a workspace.\
> Therefore, Bazel ignores any directory trees in a workspace rooted at a subdirectory containing a `WORKSPACE` file, as they form another workspace.

Spago goes by these same rules, with the difference that we do not use a separate `WORKSPACE` file, but instead use the `workspace` section of the `spago.yaml` file to define what the set of our external dependencies are, and where they come from.

This can be as simple as:
```yaml
workspace: {}
```

...which means that "this is now a workspace, and all the dependencies are going to be fetched from the Registry".

Or it can be more complex, e.g.:
```yaml
workspace:
  package_set:
    url: https://raw.githubusercontent.com/some-user/custom-package-sets/some-release/packages.json
  extra_packages:
    aff:
      path: ../my-purescript-aff
```

...which means that "this is now a workspace, and all the dependencies are going to be fetched using instructions from this custom package set (which could point to the Registry packages or somewhere else), except for the `aff` package, which is going to be fetched from the local folder `../my-purescript-aff`".

As described in the Bazel docs quoted above, the presence of a `workspace` section will denote the current folder as the root of a workspace, and Spago will recurse into its subfolders to find all the packages that are part of it - by looking for `spago.yaml` files with a `package` section - but ignore the subdirectory trees that are themselves workspaces - i.e. containing `spago.yaml` files with a `workspace` section.

### The configuration file

This section documents all the possible fields that can be present in the `spago.yaml` file, and their meaning.

```yaml
# The workspace section is one of the two sections that can be present
# at the top level. As described above, it defines where all of the
# dependencies of the project come from.
# It's optional, as it will be found only in the root of the project
# (defining the workspace), and not in any sub-package configurations,
# which will only contain the `package` section.
workspace:

  # The package_set field defines where to fetch the package set from.
  # It's optional - not defining this field will make Spago use the
  # Registry solver instead, to come up with a build plan.
  package_set:
    # It could either be a pointer to the official registry sets that
    # live at https://github.com/purescript/registry/tree/main/package-sets
    registry: 11.10.0
    # Or it could just point to a URL of a custom package set
    # See the "Custom package sets" section for more info on making one.
    # The `hash` field is actually optional and you should not add it.
    # It will be tacked on by Spago, to not download the set over and over.
    # Remove this field if you'd like Spago to fetch from the remote again.
    url: https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.7-20230207/packages.json
    hash: sha256-UZaygzoqEhhYh2lzUqbiNfOR9J+WNRc9SkQPmoo90jM=

  # This section defines any other packages that you'd like to include
  # in the build. It's optional, in case you just want to use the ones
  # coming from the Registry/package set.
  extra_packages:
    # Packages are always specified as a mapping from "package name" to
    # "where to find them", and there are quite a few ways to define
    # these locations:
    # 1) local package - it's on your filesystem but not in the workspace
    some-local-package:
      path: ../some-local-package
    # 2) remote package from the Registry
    #    Just specify the version you want to use. You can run
    #    `spago registry info` on a package to see its versions.
    #    This is useful for packages that are not in the package set,
    #    and useless when using the Registry solver.
    some-registry-package: 1.0.2
    # 3) remote package from Git
    #    The `git` and `ref` fields are required (`ref` can be a branch,
    #    a tag, or a commit hash).
    #    The `subdir` field is optional and necessary if you'd like to use
    #    a package that is not located in the root of the repo.
    #    The `dependencies` field is optional and necessary if the package
    #    does not have a `spago.yaml` file. In that case Spago will figure
    #    out the dependencies automatically.
    some-package-from-git:
        git: https://github.com/purescript/registry-dev.git
        ref: 68dddd9351f256980454bc2c1d0aea20e4d53fa9
        subdir: lib
        dependencies:
          - foo
    # 4) remote package from Git, legacy style (as the old package sets)
    #    Works like the above, but all fields are mandatory.
    legacy-package-style:
        repo: "https://github.com/purescript/purescript-prelude.git"
        version: "v6.0.1"
        dependencies:
          - prelude
          - effect
          - console

  # This section is optional, and you should specify it only if you'd like
  # to build with a custom backend, such as `purerl`.
  # Please see the "Alternate backends" section for more info.
  backend:
    # The command to run to build with this backend - required.
    cmd: "node"
    # Optional list of arguments to pass to the backend when building.
    args:
      - "arg1"
      - "arg2"
      - "arg3"

  # Optional setting to enable the "lockfile". Enabling it will generate
  # a `spago.lock` file with a cache of the build plan.
  # It's disabled by default when using package sets (because we already
  # get a stable build plan from there) and enabled by default when using
  # the Registry solver.
  # See "The lock file" section for more details.
  lock: false

  # Optional section to further customise the build.
  build_opts:
    # Directory for the compiler products - optional, defaults to `output`.
    output: "output"
    # Specify whether to censor warnings coming from the compiler
    # for files in the `.spago` directory`.
    # Optional and can be one of two possible values
    censor_library_warnings: 
      # Value 1: "all" - All warnings are censored
      all

      # Value 2: `NonEmptyArray (Either String { by_prefix :: String })`
      # - String values: 
      #      censor warnings if the code matches this code
      # - { by_prefix } values: 
      #      censor warnings if the warning's message 
      #      starts with the given text
      - CodeName
      # Note: when using `by_prefix`, use the `>` for block-string: 
      # see https://yaml-multiline.info/
      - by_prefix: >
        "Data.Map"'s `Semigroup instance`

    # Specify whether to show statistics at the end of the compilation,
    # and how verbose they should be.
    # Can be 'no-stats', 'compact-stats' (default), or 'verbose-stats',
    # which breaks down the statistics by warning code.
    stat_verbosity: "compact-stats"

# This is the only other section that can be present at the top level.
# It specifies the configuration for a package in the current folder,
# and it's optional, as one could just have a `workspace` section.
package:

  # The package name and the `dependencies` fields are the only required
  # ones, everything else is optional.
  name: my-package-name
  dependencies:
    # Dependencies can be specified in a few ways:
    # 1) just with a package name
    #    Then Spago will use the version specified in the package set,
    #    or assume the widest possible range if using the Registry solver.
    - some-package
    # 2) explicitly specify the widest range
    #    Same as above, but explicit.
    - package-with-star-range: *
    # 3) specify a defined version range
    #    The registry will then check if the package version is included
    #    in this range.
    - package-with-range: ">=1.1.1 <2.0.0"

  # Optional description for the package
  description: "a useful package"

  # Optional section to further customise the build for this package.
  build:
    # Fail the build if this package's `dependencies` field has redundant/underspecified packages.
    # Optional boolean that defaults to `false`.
    pedantic_packages: false
    
    # Specify whether to censor warnings coming from the compiler
    # for files from this package.
    # Optional and can be one of two possible values
    censor_project_warnings: 
      # Value 1: "all" - All warnings are censored
      all

      # Value 2: `NonEmptyArray (Either String { by_prefix :: String })`
      # - String values: 
      #      censor warnings if the code matches this code
      # - { by_prefix } values: 
      #      censor warnings if the warning's message 
      #      starts with the given text
      - CodeName
      # Note: when using `by_prefix`, use the `>` for block-string: 
      # see https://yaml-multiline.info/
      - by_prefix: >
        "Data.Map"'s `Semigroup instance`
    # Convert compiler warnings for files in this package's src code
    # into errors that can fail the build.
    # Optional and defaults to false
    strict:
      true

  # Optional section to specify the configuration options for bundling
  # The following options are all optional, and will default to the values
  # shown below.
  bundle:
    minify: false
    # Entrypoint for the bundle
    module: Main
    # The path of the bundle file
    outfile: "index.js"
    # Possible values are 'node' or 'browser'
    platform: browser
    # Possible values are 'app' or 'module'
    type: "app"
    # Any other flags that should be passed to the bundler.
    # You can use this to e.g. pass `--external` flags to esbuild:
    extra_args:
      - "--external:ssh2"

  # Optional section to configure the behaviour of `spago run`.
  # All the fields are optional.
  run:
    # The entrypoint for the program
    main: Main
    # List of arguments to pass to the program
    execArgs:
      - "--cli-arg"
      - "foo"

  # Optional section to configure `spago test`
  # The `main` and `dependencies` fields are required.
  test:
    main: Test.Main
    # This works like `package.dependencies`
    dependencies:
    - foo
    # Optional list of arguments to pass to the test program
    execArgs:
      - "--cli-arg"
      - "foo"

    # Fail the build if this package's test's `dependencies` field has redundant/underspecified packages.
    # Optional boolean that defaults to `false`.
    pedantic_packages: false

    # Specify whether to censor warnings coming from the compiler
    # for files from this package's test code.
    # Optional and can be one of two possible values
    censor_test_warnings: 
      # Value 1: "all" - All warnings are censored
      all

      # Value 2: `NonEmptyArray (Either String { by_prefix :: String })`
      # - String values: 
      #      censor warnings if the code matches this code
      # - { by_prefix } values: 
      #      censor warnings if the warning's message 
      #      starts with the given text
      - CodeName
      # Note: when using `by_prefix`, use the `>` for block-string: 
      # see https://yaml-multiline.info/
      - by_prefix: >
        "Data.Map"'s `Semigroup instance`
    # Convert compiler warnings for files from this package's test code
    # into errors that can fail the build.
    # Optional and defaults to false
    strict:
      true

  # Optional section for configuring the `spago publish` command.
  # If you intend to publish your package, this section becomes mandatory.
  publish:
    # The version of your package. This follows semver rules, but with no
    # prereleases - so only major.minor.patch.
    version: 1.0.0
    # The license for your source, in SPDX format: https://spdx.dev/
    license: BSD-3-Clause
    # Optional list of globs to include in the published archive, in
    # addition to the list of files that the Registry includes by default:
    # https://github.com/purescript/registry-dev/blob/master/SPEC.md#always-included-files
    include:
    - "test/**/*.purs"
    # Optional list of globs to exclude from the published archive, in
    # addition to the list of files that the Registry includes by default:
    # https://github.com/purescript/registry-dev/blob/master/SPEC.md#always-excluded-files
    # Note that the Registry will first apply the `include` list, then
    # the `exclude` one, as detailed in the specification:
    # https://github.com/purescript/registry-dev/blob/master/SPEC.md#33-manifest
    exclude:
    - "test/graphs/**/*"
    # The place where the Registry will fetch the source from.
    # This is optional since you might want to release the code without
    # publishing to the Registry, which is what this is needed for.
    location:
      # There are a few supported locations:
      # 1) Github: no URL needed, just username and the name of the repo
      #    The `subdir` field is optional, and only necessary if your
      #    package is not in the root of the repo.
      githubOwner: owners-username
      githubRepo: repo-name
      subdir: lib
      # 2) Git: any git server should work with this
      #    The `subdir` is optional as above
      url: git://someurl...
      subdir: lib
```

### The lock file

The lock file is a file that Spago can generate to cache the build plan, so that it can be reused in subsequent builds.

When using package sets it is disabled by default - since we already get a stable build plan from there - while it's enabled by default when using the Registry solver.

You can enable it manually by adding a `lock: true` field to the `workspace` section of your `spago.yaml` file, and that will keep it on regardless of which solving mode you're using.

## FAQ

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
PureScript has an fairly active [Erlang backend][purerl] among the others.

These backends are going to use different package managers for their native dependencies,
and while it's feasible for `spago` to support the backends themselves, also supporting
all the possible native package managers (and doing things like building package-sets for their
dependencies' versions) is not a scalable approach (though we might do this in the future if
there's enough demand).

So this is the reason why if you or one of your dependencies need to depend on some "native"
packages, you should run the appropriate package-manager for that (e.g. npm).

## Differences from legacy spago

### Watch mode
Spago dropped support for the --watch flag in `spago build` and `spago test`. 

VSCode users are recommended to use the [Purescript IDE](purescript-ide) extension for seamless experiences with automatic rebuilds.

Users of other editors, e.g. vim, emacs, etc., can make use of the underlying [LSP plugin](purescript-language-server).

If you want a very simple drop in replacement for `spago test --watch`, you can use a general purpose tool such as [watchexec]:
```console
watchexec -e purs,js,yaml -- spago test
```

[jq]: https://jqlang.github.io/jq/
[acme]: https://hackage.haskell.org/package/acme-everything
[pulp]: https://github.com/purescript-contrib/pulp
[stack]: https://github.com/commercialhaskell/stack
[bazel]: https://bazel.build/
[cargo]: https://github.com/rust-lang/cargo
[purerl]: https://github.com/purerl/purescript
[pursuit]: https://pursuit.purescript.org/
[registry]: https://github.com/purescript/registry
[spago-npm]: https://www.npmjs.com/package/spago
[new-issue]: https://github.com/purescript/spago/issues/new
[purescript]: https://github.com/purescript/purescript
[psc-package]: https://github.com/purescript/psc-package
[luu-monorepo]: https://danluu.com/monorepo/
[contributing]: CONTRIBUTING.md
[spago-legacy]: https://github.com/purescript/spago-legacy
[monorepo-tools]: https://monorepo.tools/
[install-esbuild]: https://esbuild.github.io/getting-started/#install-esbuild
[bazel-workspace]: https://bazel.build/concepts/build-ref
[purescript-overlay]: https://github.com/thomashoneyman/purescript-overlay
[sample-package-set]: https://github.com/purescript/registry/blob/main/package-sets/41.2.0.json
[watchexec]: https://github.com/watchexec/watchexec#quick-start
[purescript-langugage-server]: https://github.com/nwolverson/purescript-language-server
[ide-purescript]: https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript
