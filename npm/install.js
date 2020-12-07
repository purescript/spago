"use strict";

const request = require("request");
const tar = require("tar");
const version = "PACKAGE_VERSION";
const platform = { win32: "windows", darwin: "macOS" }[process.platform] || "linux";
const url = `https://github.com/purescript/spago/releases/download/${version}/${platform}-latest.tar.gz`

request.get(url).pipe(tar.x({"C": './'}));
