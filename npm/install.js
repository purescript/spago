"use strict";

// The following implementation comes from
// https://github.com/justinwoo/npm-psc-package-bin-simple/blob/4d4efa6a4e2008c8a0a71f0b189c14c31b88e47b/install.js

const request = require("request");
const tar = require("tar");
const version = "PACKAGE_VERSION";
const platform = { win32: "windows", darwin: "macOS" }[process.platform] || "linux";
const url = `https://github.com/purescript/spago/releases/download/${version}/${platform}.tar.gz`

request.get(url).pipe(tar.x({"C": './'}));
