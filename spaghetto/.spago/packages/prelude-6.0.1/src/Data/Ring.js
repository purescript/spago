export const intSub = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

export const numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};
