const newImpl = function () {
  return {};
};
export { newImpl as new };

export function peekImpl(just) {
  return function (nothing) {
    return function (k) {
      return function (m) {
        return function () {
          return {}.hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
        };
      };
    };
  };
}

export function poke(k) {
  return function (v) {
    return function (m) {
      return function () {
        m[k] = v;
        return m;
      };
    };
  };
}

const deleteImpl = function (k) {
  return function (m) {
    return function () {
      delete m[k];
      return m;
    };
  };
};
export { deleteImpl as delete };
