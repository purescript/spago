export function mutationObserver(cb) {
  return function () {
    return new MutationObserver(function (mr, mo) {
      return cb(mr)(mo)();
    });
  };
}

export function _observe(node) {
  return function (config) {
    return function (mo) {
      return function () {
        return mo.observe(node, config);
      };
    };
  };
}

export function disconnect(mo) {
  return function () {
    return mo.disconnect();
  };
}

export function takeRecords(mo) {
  return function () {
    return mo.takeRecords();
  };
}
