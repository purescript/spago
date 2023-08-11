export const _localeCompare = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (s1) {
        return function (s2) {
          var result = s1.localeCompare(s2);
          return result < 0 ? lt : result > 0 ? gt : eq;
        };
      };
    };
  };
};

export const replace = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(s1, s2);
    };
  };
};

export const replaceAll = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(new RegExp(s1.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"), "g"), s2); // eslint-disable-line no-useless-escape
    };
  };
};

export const split = function (sep) {
  return function (s) {
    return s.split(sep);
  };
};

export const toLower = function (s) {
  return s.toLowerCase();
};

export const toUpper = function (s) {
  return s.toUpperCase();
};

export const trim = function (s) {
  return s.trim();
};

export const joinWith = function (s) {
  return function (xs) {
    return xs.join(s);
  };
};
