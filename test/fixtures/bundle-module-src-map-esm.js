// output/Effect.Console/foreign.js
var log = function(s) {
  return function() {
    console.log(s);
  };
};

// output/Main/index.js
var main = /* @__PURE__ */ log("\u{1F35D}");
export {
  main
};
//# sourceMappingURL=bundle-module-src-map-esm.js.map
