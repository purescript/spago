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
