/* globals exports */
export const nan = NaN;
const isNaNImpl = isNaN;
export { isNaNImpl as isNaN };
export const infinity = Infinity;
const isFiniteImpl = isFinite;
export { isFiniteImpl as isFinite };

export function fromStringImpl(str, isFinite, just, nothing) {
  var num = parseFloat(str);
  if (isFinite(num)) {
    return just(num);
  } else {
    return nothing;
  }
}

export const abs = Math.abs;

export const acos = Math.acos;

export const asin = Math.asin;

export const atan = Math.atan;

export const atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

export const ceil = Math.ceil;

export const cos = Math.cos;

export const exp = Math.exp;

export const floor = Math.floor;

export const log = Math.log;

export const max = function (n1) {
  return function (n2) {
    return Math.max(n1, n2);
  };
};

export const min = function (n1) {
  return function (n2) {
    return Math.min(n1, n2);
  };
};

export const pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

export const remainder = function (n) {
  return function (m) {
    return n % m;
  };
};

export const round = Math.round;

export const sign = Math.sign ? Math.sign : function(x) {
  return x === 0 || x !== x ? x : (x < 0 ? -1 : 1);
};

export const sin = Math.sin;

export const sqrt = Math.sqrt;

export const tan = Math.tan;

export const trunc = Math.trunc ? Math.trunc : function(x) {
  return x < 0 ? Math.ceil(x) : Math.floor(x);
}
