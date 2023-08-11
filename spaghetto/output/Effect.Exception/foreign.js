export function showErrorImpl(err) {
  return err.stack || err.toString();
}

export function error(msg) {
  return new Error(msg);
}

export function message(e) {
  return e.message;
}

export function name(e) {
  return e.name || "Error";
}

export function stackImpl(just) {
  return function (nothing) {
    return function (e) {
      return e.stack ? just(e.stack) : nothing;
    };
  };
}

export function throwException(e) {
  return function () {
    throw e;
  };
}

export function catchException(c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
}
