/* global __dirname require exports */

import globMain from "glob";
import path from "node:path";
import { fileURLToPath } from "node:url";

export function getDirname() {
  const fileName = fileURLToPath(import.meta.url);
  const dirName = path.dirname(fileName);
  return dirName;
}

export function glob(pattern) {
  return function () {
    return globMain.sync(pattern);
  };
}

export const defaultCopyMode = 0;
