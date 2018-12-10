# spago

[![Build Status](https://travis-ci.org/spacchetti/spago.svg?branch=master)][travis-spago]

*(IPA: /ˈspaɡo/)*

<img src="https://raw.githubusercontent.com/spacchetti/logo/master/spacchetti-icon.png" height="300px" alt="spacchetti logo">

PureScript package manager and build tool powered by [Dhall][dhall] and
[Spacchetti][spacchetti] package-sets.

## What does all of this mean?

`spago` aims to tie together the UX of developing a PureScript project.  
In this Pursuit (see what I did there) it is heavily inspired by [Rust's Cargo][cargo]
and [Haskell's Stack][stack], and builds on top of ideas from existing PureScript
infrastructure and tooling, as [`psc-package`][psc-package], [`pulp`][pulp] and
[`purp`][purp].

## Installation

> Right, so how can I get this thing?

The recommended installation methods on Linux and macOS are:
- `npm install -g purescript-spago` (see the latest releases on npm [here][spago-npm])
- Download the binary from the [latest GitHub release][spago-latest-release]
- Compile from source by cloning this repo and running `stack install`

**Note #1:** we don't currently support Windows, and we're sorry for this. The
reason is that no current maintainer runs it. If you'd like to help with this
that's awesome! Get in touch by [opening an issue][spago-issues] :)

**Note #2:** we assume you already installed the [PureScript compiler][purescript].
If not, get it with `npm install -g purescript`

## Quickstart

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

Convention note: `spago` expects your source files to be in `src/` and your
test files in `test/`.

Let's take a look at the two [Dhall][dhall] configuration files that `spago` requires:
- `packages.dhall`: this file is meant to contain the *totality* of the packages
  available to your project (that is, any package you might want to import).  
  In practical terms, it pulls in a [Spacchetti][spacchetti] package-set as a base,
  and you are then able to add any package that might not be in the package set,
  or override esisting ones.
- `spago.dhall`: this is your project configuration. It includes the above package-set,
  the list of your dependencies, and any other project-wide setting that `spago` will 
  use for builds.

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
let Packages =
  { console : Package
  , effect : Package
  ...                  -- and so on, for all the packages in the package-set
  }

-- The type of the `spago.dhall` configuration is then the following:
let Config =
  { name : Text               -- the name of our project
  , dependencies : List Text  -- the list of dependencies of our app
  , packages : Packages       -- this is the type we just defined above
  }
```

## Commands

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

### Package management

We initialized a project and saw how to configure dependencies and packages, the
next step is fetching its dependencies.

If we run:

```bash
$ spago install
```

..then `spago` will download all the `dependencies` listed in `spago.dhall` (and
store them in the `.spago` folder).

### Building a project

We can then build the project and its dependencies by running:

```bash
$ spago build
```

This is just a thin layer above the PureScript compiler command `purs compile`.  
The build will produce very many JavaScript files in the `output/` folder. These
are CommonJS modules, and you can just `require()` them e.g. on Node.

However, you might want to get a single, executable file. You'd then use the following:

```bash
# You can specify the main module and the target file, or these defaults will be used
$ spago bundle --main Main --to index.js
Bundle succeeded and output file to index.js

# We can then run it with node:
$ node .
```

*However*, you might want to build a module that has been “dead code eliminated”
if you plan to make a single module of your PS exports, which can then be required
from JS.

Gotcha covered:

```bash
# You can specify the main module and the target file, or these defaults will be used
$ spago make-module --main Main --to index.js
Bundling first...
Bundle succeeded and output file to index.js
Make module succeeded and output file to index.js

> node -e "console.log(require('./index).main)"
[Function]
```

You can also test your project with `spago`:

```bash
# Test.Main is the default here, but you can override it as usual
$ spago test --main Test.Main
Build succeeded.
You should add some tests.
Tests succeeded.
```

## Can I use this with `psc-package`?

Yes! Though the scope of the integration is limited to helping your
psc-package-project to use the [Spacchetti][spacchetti] package-set.

Here's what we can do about it:

### `psc-package-local-setup`

This command creates a `packages.dhall` file in your project, that points to the
most recent Spacchetti package-set, and lets you override and add arbitrary packages.  
See the Spacchetti docs about this [here][spacchetti-local-setup].

### `psc-package-insdhall`

Do the *Ins-Dhall-ation* of the local project setup: that is, generates a local
package-set for `psc-package` from your `packages.dhall`, and points your
`psc-package.json` to it.

Functionally this is equivalent to running:

```sh
NAME='local'
TARGET=.psc-package/$NAME/.set/packages.json
mkdir -p .psc-package/$NAME/.set
dhall-to-json --pretty <<< './packages.dhall' > $TARGET
echo wrote packages.json to $TARGET
```

## FAQ

> Hey wait we have a perfectly functional `pulp` right?

Yees, however:
- `pulp` is a build tool, so you'll still have to use it with `bower` or `psc-package`.
- If you go for `bower`, you're missing out on package-sets (that is: packages versions
  that are known to be working together, saving you the headache of fitting package
  versions together all the time).
- If you use `psc-package`, you have the problem of not having the ability of overriding
  packages versions when needed, leading everyone to make their own package-set, which
  then goes unmaintained, etc.  
  Of course you can use [Spacchetti] to solve this issue, but this is exactly what
  we're doing here: integrating all the workflow in a single tool, `spago`, instead
  of having to use `pulp`, `psc-package`, `purp`, etc.
  
> So if I use `spago make-module` this thing will compile all my js deps in the file?

No. We only take care of PureScript land. In particular, `make-module` will do the
most we can do on the PureScript side of things (dead code elimination), but will
leave the `require`s still in.  
To fill them in you should use the proper js tool of the day, at the time of
writing [ParcelJS][parcel] looks like a good option.


[spacchetti]: https://github.com/spacchetti/spacchetti
[dhall]: https://github.com/dhall-lang/dhall-lang
[travis-spago]: https://travis-ci.org/spacchetti/spago
[spacchetti-local-setup]: https://spacchetti.readthedocs.io/en/latest/local-setup.html
[cargo]: https://github.com/rust-lang/cargo
[stack]: https://github.com/commercialhaskell/stack
[psc-package]: https://github.com/purescript/psc-package
[pulp]: https://github.com/purescript-contrib/pulp
[purp]: https://github.com/justinwoo/purp
[parcel]: https://parceljs.org
[purescript]: https://github.com/purescript/purescript
[spago-npm]: https://www.npmjs.com/package/purescript-spago
[spago-latest-release]: https://github.com/spacchetti/spago/releases/latest
[spago-issues]: https://github.com/spacchetti/spacchetti-cli/issues
