"use strict";

const https = require("follow-redirects").https;
const tar = require("tar");
const shell = require("shelljs");
const version = "PACKAGE_VERSION";
const platform = { win32: "windows", darwin: "osx" }[process.platform] || "linux";

https.get(
    `https://github.com/spacchetti/spago/releases/download/${version}/${platform}.tar.gz`,
    res => res.pipe(
        tar.x({"C": './'}).on("finish", () => {
            if (shell.test("-f", "./spago")) {
                shell.mv("./spago", "./spago.exe")
            }
        })
    )
);
