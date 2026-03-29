#!/usr/bin/env bash

set -euo pipefail

# Please use this script to cut new releases.
# It will bump the version, commit, cut a tag and push it.
# CI takes care of building a release version, and publishing it to NPM.

# Fail if not on master
BRANCH=$(git branch --show-current)
if [ "${BRANCH}" != "master" ]; then
  echo "Please checkout master branch"
  exit 1
fi

# Check for uncommitted changes
if ! git diff-index --quiet HEAD --; then
  echo "You have uncommitted changes. Please commit or stash them first."
  exit 1
fi

git pull

# Get version argument
if [ $# -eq 0 ]; then
  echo "Usage: ./release.sh <version>"
  echo "Example: ./release.sh 1.0.0"
  echo "         ./release.sh patch"
  echo "         ./release.sh minor"
  exit 1
fi

NEW_VERSION=$1

# Fail if tag already exists (for explicit versions)
if [[ ! "$NEW_VERSION" =~ ^(patch|minor|major)$ ]]; then
  if git rev-parse "${NEW_VERSION}" >/dev/null 2>&1; then
    echo "Tag '${NEW_VERSION}' already exists."
    exit 1
  fi
fi

echo "Bumping version to '${NEW_VERSION}'.."

# npm version:
# 1. Updates package.json
# 2. Runs the "version" script (syncs version to spago.yaml files)
# 3. Commits the changes
# 4. Creates a git tag
npm version "${NEW_VERSION}" -m "Release %s"

echo ""
echo "Pushing to origin..."
git push origin master --tags

echo ""
echo "Done! CI will now build, test, and publish to npm."
