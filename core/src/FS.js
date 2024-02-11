import fs from 'fs-extra';
const { moveSync, ensureFileSync } = fs;
import path from 'path';

export const moveSyncImpl = (source) => (destination) => () => moveSync(source, destination);

export const ensureFileSyncImpl = (file) => () => ensureFileSync(file);

export const cpImpl = (source) => (dest) => () => fs.copySync(source, dest);

// This takes a "basepath" and a "path", and returns the segments of difference between them.
// So e.g. if basepath = "/a/b/c" and path = "/a/b/c/d/e", it will return ["d", "e"]
function getPathDifference(basepath, pathToCompare) {
  const normalizedBase = path.normalize(basepath).split(path.sep);
  const normalizedPath = path.normalize(pathToCompare).split(path.sep);
  return normalizedPath.slice(normalizedBase.length);
}

// Same as above, but return all the paths in between
export function getInBetweenPathsImpl(basepath, pathToCompare) {
  const difference = getPathDifference(basepath, pathToCompare);
  let currentPath = path.normalize(basepath);
  const allPaths = [];

  for (let segment of difference) {
    currentPath = path.join(currentPath, segment);
    allPaths.push(currentPath);
  }

  return allPaths;
}

