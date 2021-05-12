#!/usr/bin/env bash

set -euo pipefail

# Please use this script to cut new releases.
# It will create a tag and push it, which will trigger CI to create a release,
# build release binaries, push them to the release, and publish a new version on NPM

# Fail if we are not on trunk
BRANCH=$(git branch --show-current)
if [ "${BRANCH}" != "master" ]; then
  echo "Please checkout master branch"
  exit 1;
fi
git pull

NEW_TAG=$(./scripts/get-version)

# Fail if the new tag already exists
# Note: exit status of the command is the conditional
if git rev-parse "${NEW_TAG}" >/dev/null 2>&1; then
  echo "Tag '${NEW_TAG}' already exists, please bump the version in spago.cabal";
  exit 1;
fi

echo "Creating and pushing new tag '${NEW_TAG}'.."

git tag "${NEW_TAG}"

git push origin "${NEW_TAG}"

echo "Done."
