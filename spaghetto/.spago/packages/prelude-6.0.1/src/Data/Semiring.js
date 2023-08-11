export const intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

export const intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

export const numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

export const numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};
