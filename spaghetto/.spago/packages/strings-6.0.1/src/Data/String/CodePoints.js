/* global Symbol */

var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator =
  typeof Symbol !== "undefined" &&
  Symbol != null &&
  typeof Symbol.iterator !== "undefined" &&
  typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";

export const _unsafeCodePointAt0 = function (fallback) {
  return hasCodePointAt
    ? function (str) { return str.codePointAt(0); }
    : fallback;
};

export const _codePointAt = function (fallback) {
  return function (Just) {
    return function (Nothing) {
      return function (unsafeCodePointAt0) {
        return function (index) {
          return function (str) {
            var length = str.length;
            if (index < 0 || index >= length) return Nothing;
            if (hasStringIterator) {
              var iter = str[Symbol.iterator]();
              for (var i = index;; --i) {
                var o = iter.next();
                if (o.done) return Nothing;
                if (i === 0) return Just(unsafeCodePointAt0(o.value));
              }
            }
            return fallback(index)(str);
          };
        };
      };
    };
  };
};

export const _countPrefix = function (fallback) {
  return function (unsafeCodePointAt0) {
    if (hasStringIterator) {
      return function (pred) {
        return function (str) {
          var iter = str[Symbol.iterator]();
          for (var cpCount = 0; ; ++cpCount) {
            var o = iter.next();
            if (o.done) return cpCount;
            var cp = unsafeCodePointAt0(o.value);
            if (!pred(cp)) return cpCount;
          }
        };
      };
    }
    return fallback;
  };
};

export const _fromCodePointArray = function (singleton) {
  return hasFromCodePoint
    ? function (cps) {
      // Function.prototype.apply will fail for very large second parameters,
      // so we don't use it for arrays with 10,000 or more entries.
      if (cps.length < 10e3) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton).join("");
    }
    : function (cps) {
      return cps.map(singleton).join("");
    };
};

export const _singleton = function (fallback) {
  return hasFromCodePoint ? String.fromCodePoint : fallback;
};

export const _take = function (fallback) {
  return function (n) {
    if (hasStringIterator) {
      return function (str) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done) return accum;
          accum += o.value;
        }
        return accum;
      };
    }
    return fallback(n);
  };
};

export const _toCodePointArray = function (fallback) {
  return function (unsafeCodePointAt0) {
    if (hasArrayFrom) {
      return function (str) {
        return Array.from(str, unsafeCodePointAt0);
      };
    }
    return fallback;
  };
};
