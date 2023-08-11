export const boolConj = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

export const boolDisj = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

export const boolNot = function (b) {
  return !b;
};
