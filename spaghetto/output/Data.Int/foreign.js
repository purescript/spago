export const fromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      /* jshint bitwise: false */
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

export const toNumber = function (n) {
  return n;
};

export const fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");

      return function (s) {
        /* jshint bitwise: false */
        if (pattern.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
};

export const toStringAs = function (radix) {
  return function (i) {
    return i.toString(radix);
  };
};


export const quot = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

export const rem = function (x) {
  return function (y) {
    return x % y;
  };
};

export const pow = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return Math.pow(x,y) | 0;
  };
};
