#!/usr/bin/env node
import __module from 'module';import __path from 'path';import __url from 'url';const require = __module.createRequire(import.meta.url);const __dirname = __path.dirname(__url.fileURLToPath(import.meta.url));const __filename=new URL(import.meta.url).pathname

// output/Effect.Console/foreign.js
var log = function(s) {
  return function() {
    console.log(s);
  };
};

// output/Main/index.js
var main = /* @__PURE__ */ log("\u{1F35D}");

// <stdin>
main();
