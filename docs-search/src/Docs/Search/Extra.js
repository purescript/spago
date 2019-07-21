/* global exports require */

var glob = require('glob');

exports.glob = function (pattern) {
  return function () {
    return glob.sync(pattern);
  };
};
