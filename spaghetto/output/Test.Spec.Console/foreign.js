/* global exports */
"use strict";

// module Test.Spec.Console

export function write(s) {
  return function () {
    try {
      process.stdout.write(s);
    }
    catch (e) {}
  };
}
