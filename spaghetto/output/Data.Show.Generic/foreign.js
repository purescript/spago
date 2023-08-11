export const intercalate = function (separator) {
  return function (xs) {
    return xs.join(separator);
  };
};
