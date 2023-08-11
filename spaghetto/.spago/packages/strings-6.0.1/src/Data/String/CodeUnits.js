export const fromCharArray = function (a) {
  return a.join("");
};

export const toCharArray = function (s) {
  return s.split("");
};

export const singleton = function (c) {
  return c;
};

export const _charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

export const _toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

export const length = function (s) {
  return s.length;
};

export const countPrefix = function (p) {
  return function (s) {
    var i = 0;
    while (i < s.length && p(s.charAt(i))) i++;
    return i;
  };
};

export const _indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

export const _indexOfStartingAt = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

export const _lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

export const _lastIndexOfStartingAt = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

export const take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

export const drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};

export const slice = function (b) {
  return function (e) {
    return function (s) {
      return s.slice(b,e);
    };
  };
};

export const splitAt = function (i) {
  return function (s) {
    return { before: s.substring(0, i), after: s.substring(i) };
  };
};
