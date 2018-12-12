#!/usr/bin/env python3

import os
from utils import expect_success, expect_failure, fail


## spago psc-package-local-setup

expect_success(
    ['spago', 'psc-package-local-setup'],
    "Local setup failed on first run."
)

if not os.path.exists('packages.dhall'):
    fail("Failed to produce packages.dhall from local setup")

expect_failure(
    ['spago', 'psc-package-local-setup'],
    "Running local setup twice should have caused an error with an existing setup"
)


## spago psc-package-insdhall

expect_success(
    ['spago', 'psc-package-insdhall'],
    "Insdhall should have run successfully"
)

if not os.path.exists('.psc-package/local/.set/packages.json'):
    fail("Insdhallation failed to produce a package set")

expect_success(
    ['psc-package', 'build'],
    "Psc-Package build should have worked successfully"
)


## spago psc-package clean

expect_success(
    ['spago', 'psc-package-clean'],
    "Clean should have completed successfully"
)

if os.path.exists('.psc-package'):
    fail("Clean did not clean up .psc-package")


## Cleanup after tests

os.remove('packages.dhall')
expect_success(["git", "checkout", "--", "psc-package.json"], "Git cleanup success")
