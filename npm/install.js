"use strict";

// The following implementation comes from
// https://github.com/justinwoo/npm-psc-package-bin-simple/blob/4d4efa6a4e2008c8a0a71f0b189c14c31b88e47b/install.js

const https = require("follow-redirects").https;
const tar = require("tar");
const version = "PACKAGE_VERSION";
const platform = { win32: "windows", darwin: "osx" }[process.platform] || "linux";

https.get(
    `https://github.com/spacchetti/spago/releases/download/${version}/${platform}.tar.gz`,
    res => res.pipe(tar.x({"C": './'}))
);
