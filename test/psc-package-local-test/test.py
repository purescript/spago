#!/usr/bin/env python3

import subprocess
import os.path

local_setup = subprocess.run(['spago', 'psc-package-local-setup'], capture_output=True)
if local_setup.returncode != 0:
    print("Local setup failed on first run.")
    exit(1)

if not os.path.exists('packages.dhall'):
    print("Failed to produce packages.dhall from local setup")
    exit(1)

override_needed = subprocess.run(['spago', 'psc-package-local-setup'], capture_output=True)
if override_needed.returncode != 1:
    print("Running local setup twice should have caused an error with an existing setup")
    exit(1)

insdhall = subprocess.run(['spago', 'psc-package-insdhall'], capture_output=True)
if insdhall.returncode != 0:
    print("Insdhall should have run successfully")
    exit(1)

if not os.path.exists('.psc-package/local/.set/packages.json'):
    print("Insdhallation failed to produce a package set")
    exit(1)

psc_package = subprocess.run(['psc-package', 'build'], capture_output=True)
if psc_package.returncode != 0:
    print("Psc-Package build should have worked successfully")
    exit(1)

clean = subprocess.run(['spago', 'psc-package-clean'], capture_output=True)
if clean.returncode != 0:
    print("Clean should have completed successfully")
    exit(1)

if os.path.exists('.psc-package'):
    print("Clean did not clean up .psc-package")
    exit(1)
