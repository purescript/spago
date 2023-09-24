export const concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

export const concatArray = function (xs) {
  return function (ys) {
    if (xs.length === 0) return ys;
    if (ys.length === 0) return xs;
    return xs.concat(ys);
  };
};
