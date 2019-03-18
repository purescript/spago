#!/usr/bin/env python3

import os
import json
import time
import shutil
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
expect_success(['rmdir', 'src'], "")

expect_success(
    ['spago', 'init', '-f'],
    "Spago should always succeed in doing init with force"
)



# Here we check that spago can import a project from psc-package
with open('psc-package.json', 'w') as pscfile:
    data = { "name": "aaa", "depends": [ "prelude" ], "set": "foo", "source": "bar" }
    json.dump(data, pscfile)
expect_success(
    ['spago', 'init', '-f'],
    "Spago should import config from psc-package"
)
shutil.copy2('spago.dhall', 'spago-psc-success.dhall')
check_fixture('spago-psc-success.dhall')


# But should not import dependencies that are not in the package-set
with open('psc-package.json', 'w') as pscfile:
    data = { "name": "aaa", "depends": [ "prelude", "foo", "bar" ], "set": "foo", "source": "bar" }
    json.dump(data, pscfile)
expect_success(
    ['spago', 'init', '-f'],
    "Spago should not import dependencies that are not in the package-set"
)
shutil.copy2('spago.dhall', 'spago-psc-failure.dhall')
check_fixture('spago-psc-failure.dhall')



## spago install

# Run `install` once and kill it soon to simulate failure
run_for(0.5, ['spago', 'install', '-j', '3'])

# Sleep for some time, as the above might take time to cleanup old processes
time.sleep(1)

expect_success(
    ['spago', 'install', '-j', '10'],
    "Subsequent installs should succeed anyways"
)

expect_success(
    ['spago', 'install', '-j', '10', 'simple-json', 'foreign'],
    "Spago should be able to add dependencies"
)
os.rename('spago.dhall', 'spago-install-success.dhall')
check_fixture('spago-install-success.dhall')


expect_success(
    ['spago', 'init', '-f'],
    "Spago should always succeed in doing init with force"
)

expect_failure(
    ['spago', 'install', 'foobar'],
    "Spago should not add dependencies that are not in the package set"
)
shutil.copy2('spago.dhall', 'spago-install-failure.dhall')
check_fixture('spago-install-failure.dhall')




## spago build

expect_success(
    ['spago', 'build'],
    "Spago should build successfully"
)

expect_success(
    ['spago', 'build', '--', '-o myOutput'],
    "Spago should pass options to purs"
)
assert os.path.isdir('myOutput') == True

os.mkdir('another_source_path')
os.rename('src/Main.purs', 'another_source_path/Main.purs')
expect_success(
    ['spago', 'build', '--path', 'another_source_path/*.purs'],
    "Spago should build successfully sources included from custom path"
)
os.rename('another_source_path/Main.purs', 'src/Main.purs')




## spago test

#### Note: here we check that the tests effectively run, by matching on the expected output

expect_success(
    ['spago', 'test'],
    "Spago should test successfully",
    "test-output.txt"
)


## spago run

expect_success(
    ['spago', 'run'],
    "Spago should run successfully",
    "run-output.txt"
)


## spago bundle
shutil.rmtree("./output") ## Remove output to ensure bundle builds as well as bundles

expect_success(
    ['spago', 'bundle', '--to', 'bundle.js'],
    "Spago should bundle successfully"
)

check_fixture('bundle.js')




## spago make-module
shutil.rmtree("./output") ## Remove output to ensure bundle builds as well as bundles

expect_success(
    ['spago', 'make-module', '--to', 'module.js'],
    "Spago should successfully make a module"
)

check_fixture('module.js')





## Cleanup after tests

expect_success(
    ['rm', '-rf', '.spago', 'src', 'test', 'packages.dhall', 'spago.dhall', 'bundle.js',
     'module.js', 'output', 'myOutput', 'another_source_path', 'psc-package.json',
     'spago-psc-success.dhall', 'spago-psc-failure.dhall', 'spago-install-success.dhall',
     'spago-install-failure.dhall'],
    "Cleanup should empty the project folder"
)
