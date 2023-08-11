export function eventListener(fn) {
  return function () {
    return function (event) {
      return fn(event)();
    };
  };
}

export function addEventListenerWithOptions(type) {
  return function (listener) {
    return function (options) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, options);
        };
      };
    };
  };
}

export function addEventListener(type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, useCapture);
        };
      };
    };
  };
}

export function removeEventListener(type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          return target.removeEventListener(type, listener, useCapture);
        };
      };
    };
  };
}

export function dispatchEvent(event) {
  return function (target) {
    return function () {
      return target.dispatchEvent(event);
    };
  };
}
