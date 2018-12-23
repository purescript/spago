#!/usr/bin/env python3

import os
from utils import expect_success, expect_failure, fail, run_for, check_fixture


## spago init

expect_success(
    ['spago', 'init'],
    "Spago should have set up a project"
)

expect_failure(
    ['spago', 'init'],
    "Spago should refuse to overwrite an existing project without -f"
)

# Here we check that spago does not overwrite any source files we might have
expect_success(
    ['rm', 'spago.dhall', 'packages.dhall'],
    "Cleaning of config files should succeed"
)
with open('src/Main.purs', 'w') as f:
    f.write("Something")
expect_success(
    ['spago', 'init'],
    "Spago should not overwrite files when initing a project"
)
with open('src/Main.purs', 'r') as f:
    assert f.read() == 'Something'
expect_success(['rm', 'src/Main.purs'], "")

expect_success(
    ['spago', 'init', '-f'],
    "Spago should always succeed in doing init with force"
)

## spago install

# Run `install` once and kill it soon to simulate failure
run_for(0.5, ['spago', 'install', '-j', '3'])

expect_success(
    ['spago', 'install'],
    "Subsequent installs should succeed anyways"
)


## spago build

expect_success(
    ['spago', 'build'],
    "Spago should build successfully"
)


## spago test

expect_success(
    ['spago', 'test'],
    "Spago should test successfully"
)


## spago bundle

expect_success(
    ['spago', 'bundle', '--to', 'bundle.js'],
    "Spago should bundle successfully"
)

check_fixture('bundle.js')


## spago make-module

expect_success(
    ['spago', 'make-module', '--to', 'module.js'],
    "Spago should successfully make a module"
)

check_fixture('module.js')


## Cleanup after tests

expect_success(
    ['rm', '-rf', '.spago', 'src', 'test', 'packages.dhall', 'spago.dhall', 'bundle.js', 'module.js', 'output'],
    "Cleanup should empty the project folder"
)
