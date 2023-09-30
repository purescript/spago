#!/usr/bin/env node
(() => {
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
})();
//# sourceMappingURL=bundle-app-map.js.map
