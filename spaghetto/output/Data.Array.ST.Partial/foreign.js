export const peekImpl = function (i) {
  return function (xs) {
    return function () {
      return xs[i];
    };
  };
};

export const pokeImpl = function (i) {
  return function (a) {
    return function (xs) {
      return function () {
        xs[i] = a;
      };
    };
  };
};
