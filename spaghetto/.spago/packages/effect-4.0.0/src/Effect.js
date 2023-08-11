export const pureE = function (a) {
  return function () {
    return a;
  };
};

export const bindE = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

export const untilE = function (f) {
  return function () {
    while (!f());
  };
};

export const whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
    };
  };
};

export const forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

export const foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};
