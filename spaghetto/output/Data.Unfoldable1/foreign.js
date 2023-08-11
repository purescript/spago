export const unfoldr1ArrayImpl = function (isNothing) {
  return function (fromJust) {
    return function (fst) {
      return function (snd) {
        return function (f) {
          return function (b) {
            var result = [];
            var value = b;
            while (true) { // eslint-disable-line no-constant-condition
              var tuple = f(value);
              result.push(fst(tuple));
              var maybe = snd(tuple);
              if (isNothing(maybe)) return result;
              value = fromJust(maybe);
            }
          };
        };
      };
    };
  };
};
