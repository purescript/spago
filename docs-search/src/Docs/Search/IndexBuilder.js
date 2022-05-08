/* global __dirname require exports */

import globMain from "glob";

export function getDirname () {
  return __dirname;
};

export function glob (pattern) {
  return function () {
    return globMain.sync(pattern);
  };
};
