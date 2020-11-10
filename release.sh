#!/usr/bin/env bash

set -euo pipefail

# Please use this script to cut new releases.
# It will create a tag and push it, which will trigger CI to create a release,
# build release binaries, push them to the release, and publish a new version on NPM

NEW_TAG=$(./scripts/get-version)

# Fail if the new tag already exists
if git rev-parse "${NEW_TAG}" >/dev/null 2>&1; then
  echo "Tag '${NEW_TAG}' already exists, please bump the version in package.yaml";
  exit 1;
fi

echo "Creating and pushing new tag '${NEW_TAG}'.."

git tag "${NEW_TAG}"

git push origin "${NEW_TAG}"

echo "Done."