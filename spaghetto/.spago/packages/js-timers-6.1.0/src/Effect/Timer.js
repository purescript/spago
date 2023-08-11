/* no-redeclare global exports */
export function setTimeoutImpl(ms) {
  return function (fn) {
    return function () {
      return setTimeout(fn, ms);
    };
  };
}

export function clearTimeoutImpl(id) {
  return function () {
    clearTimeout(id);
  };
}

export function setIntervalImpl(ms) {
  return function (fn) {
    return function () {
      return setInterval(fn, ms);
    };
  };
}

export function clearIntervalImpl(id) {
  return function () {
    clearInterval(id);
  };
}
