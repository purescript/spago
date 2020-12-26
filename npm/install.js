"use strict";

const request = require("request");
const tar = require("tar");
const version = "PACKAGE_VERSION";
const platform = { win32: "Windows", darwin: "macOS" }[process.platform] || "Linux";
const url = `https://github.com/purescript/spago/releases/download/${version}/${platform}.tar.gz`

request.get(url).pipe(tar.x({"C": './'}));
