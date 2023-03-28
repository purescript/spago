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

## Spago.yml

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
    #   2. the package, using the widest possible vesrion range
    - packageName: "*"
    #   2. the package, using a specified vesrion range
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
```
