var refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

export const eqBooleanImpl = refEq;
export const eqIntImpl = refEq;
export const eqNumberImpl = refEq;
export const eqCharImpl = refEq;
export const eqStringImpl = refEq;

export const eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};
