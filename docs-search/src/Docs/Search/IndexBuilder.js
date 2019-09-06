/* global __dirname require exports */

var path = require('path');
var glob = require('glob');

exports.getDirname = function () {
  return __dirname;
};

exports.glob = function (pattern) {
  return function () {
    return glob.sync(pattern);
  };
};
