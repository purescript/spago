"use strict";

export function _startsWith(subs) {
  return function (str) {
    return str.startsWith(subs)
  }
}

export function _endsWith(subs) {
  return function (str) {
    return str.endsWith(subs)
  }
}
