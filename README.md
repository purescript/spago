# spacchetti-cli

**Attention: See the Spacchetti docs first at <https://spacchetti.readthedocs.io/>**

A CLI for Spacchetti that does some stuff, but does not do anything that Psc-Package already does.

## Features

### LocalSetup

Do the boilerplate of the local project setup to override and add arbitrary packages
See the Spacchetti docs about this here: <https://spacchetti.readthedocs.io/en/latest/local-setup.html>

### InsDhall

Do the Ins-Dhall-ation of the local project setup, equivalent to:
```sh
NAME='local'
TARGET=.psc-package/$NAME/.set/packages.json
mkdir -p .psc-package/$NAME/.set
dhall-to-json --pretty <<< './packages.dhall' > $TARGET
echo wrote packages.json to $TARGET
```
