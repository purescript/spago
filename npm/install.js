"use strict";

const fetch = require("make-fetch-happen");
const tar = require("tar");
const version = "PACKAGE_VERSION";
const platform = { win32: "Windows", darwin: "macOS" }[process.platform] || "Linux";
const url = `https://github.com/purescript/spago/releases/download/${version}/${platform}.tar.gz`

fetch(url).then(res => res.body.pipe(tar.x({"C": './'})));
