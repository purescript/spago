## Internals

### The `spago-curator` tool

While we publish only the `spago` binary, there is another executable that is built together
with it, and that's `spago-curator`.

Its purpose is to assist in the automation of certain tasks that make life easier (both
for maintainers and users). You can think of it as a glorified Perl script.

It requires a GitHub token in the `SPACCHETTIBOTTI_TOKEN` and a configured ssh key, 
authenticated to all the repos it pushes to.

All its operations are run as the [spacchettibotti][spacchettibotti] user.

Once started, every 1h will do the following things:
- check if there's a new tag out for package-sets. If yes, it opens a PR to `spago` to update it
- crawl GitHub downloading the list of all tags and commits for every repo in the set.  
  These will be put in a `metadataV1.json` file, in [the `package-sets-metadata` repo][package-sets-metadata].
  This file is used so that `spago` can rely on it for information about "is this ref immutable",
  effectively enabling the possibility of a global cache.
- check if the latest tag for every package is the latest tag in the set.
  If not, it updates it locally, tries to verify the new set, and if everything is fine it
  opens a PR to the [`package-sets` repo](https://github.com/purescript/package-sets)
  
