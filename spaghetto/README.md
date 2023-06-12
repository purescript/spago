# Spago Next

The next iteration of [Spago](https://github.com/purescript/spago).

This project is still _extremely_ alpha-quality software, use at your own risk.

## Installing

> NOTE! The npm package will install an executable called `spago`, so be careful not to mix it up with the Haskell-based one.

```bash
npm install -g spago@next
```

## Developing

```bash
# Install dependencies
npm ci
# Bootstrap
spago bundle -p spago-bin
# From now on you can build with the local files in the output folder, e.g.:
./bin/index.dev.js bundle -p spago-bin
# Or from the built bundle:
./bin/bundle.js bundle -p spago-bin
```

## The `spago.yml` configuration file

In general, repositories are not always used to store a single project per repository. Nowadays, there are concepts like [monorepos](https://monorepo.tools/).

To better support [monorepos and multirepos/polyrepos](https://monorepo.tools/), Spago works in terms of "workspaces".

| Term | Meaning | Example |
| - | - | - |
| **Package** | a library or script/application | -- |
| **Dependencies** | libraries used by a **package** | A remote package (e.g. `prelude`) or a local package (e.g. a package in a monorepo) |
| **Backend** | the tool to use to compile PureScript source code to some target language. When this is not defined, source code is compiled to JavaScript via `purs`. | <ul><li>For Erlang, [`purerl`](https://github.com/purerl/purerl)</li><li>For optimized JS, [`purs-backend-es`]()</li><li>... etc.</li></ul> |
| **Workspace** | indicates which **package(s)** using some common source of **dependencies** can be compiled to a language via te same **backend** tool. |

`spago.yml` files can be used primarily in three different ways depending on whether the `workspace` and/or `package` field(s) are used:

| -- | has `workspace` | lacks `workspace` |
| - | - | - |
| has `package` | <ul><li>**Concept**: monorepo root with a single primary application</li><li>**Location**: for normal repos or monorepos, stored in the root directory. For polyrepos, stored in the root folder of that project</li><li>**Usage**: for polyrepos, defines a single backend-specific output target (e.g. Erlang server)</li></ul> | <ul><li>**Concept**: defines another package in a monorepo/polyrepo.</li><li>**Location**: stored in a subdirectory of a folder containing a workspace config file</li><li>**Usage**: usually defines a shared local library used by other local packages or defines a local script to automate some task</li></ul> |
| lacks `package` | <ul><li>**Concept**: monorepo with no single primary application</li><li>**Location**: for monorepos, stored in the root directory.</li><li>**Usage**: defines the backend and source of dependencies used by all other packages in the workspace</li></ul>  | invalid `spago.yml` file |

Each idea is further illustrated below:

<details>
<summary>A "normal" repo: a single repository containing one distinct project</summary>

Given this project structure...
```
root
  /spago.yml
  /src/Main.purs
```

... the `spago.yml` file would look like this:
```yml
package:
  name: my-random-number-game
  dependencies:
    - prelude
    - effect
    - console

workspace:
  package_set:
    registry: 11.10.0
```

In other words, there is only 1 package named `my-random-number-game` for this workspace.

</details>

<details>
<summary>A monorepo repo with no single primary application</summary>

Given this project structure...
```sh
root
  /spago.yml
#  /src/Main.purs # if the repo has a single primary application
  /core
    /spago.yml
    /src/Types.purs
  /lib1
    /spago.yml
    /src/Types.purs
  /script1
    /spago.yml
    /src/Script1/Main.purs
```

... the `spago.yml` files might look like this:

```yml
# root/spago.yml

# if the repo has a single primary application, this is uncommented
#package:
#  name: core
#  dependencies:
#    - prelude

workspace:
  package_set:
    registry: 11.10.0
```

```yml
# core/spago.yml
package:
  name: core
  dependencies:
    - prelude
```

```yml
# lib/spago.yml
package:
  name: lib1
  dependencies:
    - prelude
    - core
```

```yml
# core/spago.yml
package:
  name: app1
  dependencies:
    - prelude
    - core
```

```yml
# core/spago.yml
package:
  name: app2
  dependencies:
    - prelude
    - core
    - lib1
```

</details>


<details>
<summary>A polyrepo: loosely-connected projects</summary>

Given this project structure...
```diff
root
  /erlang
    /spago.yml
    /src/Main.purs
    /scripts
      /spago.yml
      /src/Scripts/Main.purs
  /node
    /spago.yml
    /src/Main.purs
```

... the `spago.yml` files might look like this:

```yml
# root/erlang/spago.yml
package:
  name: erlang-app
  dependencies:
    - prelude

workspace:
  package_set:
    registry: 11.10.0
  backend:
    cmd: purerl
```

```yml
# root/node/spago.yml
package:
  name: node-app
  dependencies:
    - prelude

workspace:
  package_set:
    registry: 11.10.0
  backend:
    cmd: purs-backend-es
```

</details>

### Finding Packages in a Workspace

- A "workspace" `spago.yml` file is one that *uses* the `workspace` field.
- A "non-workspace" `spago.yml` file is one that *omits* the `workspace` field.

Spago uses "workspace" `spago.yml` files to determine what are all of the packages one could build. Given the following project structure...

```sh
foo
  /spago.yml # workspace config file; defines package `foo`
  /bar/spago.yml # workspace config file; defines package `bar`
  /bar/something.spago.yml # non-workspace file; defines package `something`
  /baz/spago.yml # non-workspace config file; defines package `baz`
```

<details>
<summary>The `foo` and `baz` packages can be built when `foo` is the current working directory.</summary>

When ran from the directory `foo`, Spago will search for a "workspace" config file in the current directory. It finds `foo/spago.yml` and sees that it defines the package `foo`. It will then recurse into all subdirectories of `foo`.
- If it finds another "workspace" config file in these subdirectories (e.g. `foo/bar/spago.yml`), it ignores that entire subdirectory. So packages from `foo/bar/spago.yml` and `foo/bar/something/spago.yml` are excluded
- If it finds a "non-workspace" config files (e.g. `foo/baz/spago.yml`), it includes those packages. 

At the end of this process, `foo` and `baz` are the only packages considered.

</details>

### `spago.yml` fields and their meaning

```yml
# optional
package:
  # required, String
  name: my-package-name

  # optional, String
  description: "a useful package"

  # required
  dependencies:
    # One of three options
    #   1. just the package
    - packageName
    #   2. the package, using the widest possible version range
    - packageName: "*"
    #   2. the package, using a specified version range
    - packageName: ">=1.1.1 <2.0.0"

# optional
  bundle:
    # optional, Boolean, whether to minify
    minify: true
    # optional, String, the module name to bundle
    module: Main
    # optional, String, the file to which to output
    outfile: "index.js"
    # optional, "node" or "browser"
    platform: "browser"
    # optional, "app" or "module"
    type: "app"

# optional
  run:
    # optional, String, the module that has the `main :: Effect Unit` function
    main: "Main"
    # optional, Array String, args to pass to the program
    execArgs: 
      - "--cli-arg"
      - "foo"

  # optional
  test:
    # required, String, the module that has the `main :: Effect Unit` function
    main: "Test.Main"
    # optional, Array String, args to pass to the program
    execArgs: 
      - "--cli-arg"
      - "foo"
    # required, see `dependencies` above, additional dependencies for the test
    dependencies:
      - foo

  # optional
  publish:
    # required, Version
    version: 1.0.0
    # required, SPDX-license
    license: BSD-3-Clause
    # optional, location
    location:
      githubOwner: owner
      githubRepo: repo

# Optional
workspace:
  # optional
  package_set:
    # either registry address
    # https://raw.githubusercontent.com/purescript/registry/main/package-sets/11.10.0.json
    registry: 11.10.0
    # or package set address
    url: "https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.7-20230207/packages.json"
    # optional hash
    hash: "sha-356: "

  # optional
  extra_packages:
    # either 1) a remote package, which is one of the following
    #   1a. registry version
    - packageName: ">=1.0.0 <2.0.0"
    #   1b. git package
    - packageName:
        # required, the git repo
        git: https://github.com/purescript/registry-dev.git
        # required, branch, commit, or tag
        ref: 68dddd9351f256980454bc2c1d0aea20e4d53fa9
        # Optional, the folder within the repo to use for source globs
        subdir: lib
        # optional, see dependencies above
        dependencies: 
          - foo
    #   1c. legacy package (Haskell spago.dhall)
    - packageName:
        repo: "https://github.com/purescript/purescript-prelude.git"
        version: "v6.0.1"
        dependencies:
          - prelude
          - effect
          - console

    # or 2) a local package
    - packageName: 
        path: "path/to/file"
  
  # optional
  backend:
    # required, string, the name of the backend binary to use
    cmd: "node"
    # optional, Array String
    args:
      - "arg1"
      - "arg2"
      - "arg3"

  # optional
  build_opts:
    # optional, the `purs compile` output directory
    output: "output"
    # optional, Boolean, fail the build if `spago.yml` has redundant/missing packages
    pedantic_packages: false
    # optional, whether to censor warnings from dependency sources,
    # project sources, both, or none
    censor_warnings:
      # one of 4 values:
      #   1. Both
      "all"
      #   2. dependency only
      "dependency"
      #   3. project only
      "project"
      #   4. do not censor warnings
      "none"
    # optional, NonEmptyArray, censor specific codes
    censor_codes:
      - ShadowedName
    # optional, NonEmptyArray, only show specific codes
    filter_codes:
      - ShadowedName
    # optional, whether to show statistics at the end
    # of warning/error output and how much informaiton
    stat_verbosity:
      # One of 3 values
      #   1. Don't show it
      "no-stats"
      #   2. Show it and only sum the total warnings/errors
      "compact-stats"
      #   3. Show it and show total warnings/errors by code
      "verbose-stats"
    # optional, boolean, whether to show the source code
    # corresponding to the error's location
    show_source: true
    # optional, boolean, counts compiler warnings as compiler errors
    strict: false
    # optional, Boolean String, persist compiler warnings
    stash: true
```

### FAQs

#### Why switch from `spago.dhall` to `spago.yml`?

See `some link here...`.
