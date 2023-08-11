function newSTArray () {
  return [];
}
export { newSTArray as new };

export const peekImpl = function (just) {
  return function (nothing) {
    return function (i) {
      return function (xs) {
        return function () {
          return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
        };
      };
    };
  };
};

export const poke = function (i) {
  return function (a) {
    return function (xs) {
      return function () {
        var ret = i >= 0 && i < xs.length;
        if (ret) xs[i] = a;
        return ret;
      };
    };
  };
};

export const length = function (xs) {
  return function () {
    return xs.length;
  };
};

export const popImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function () {
        return xs.length > 0 ? just(xs.pop()) : nothing;
      };
    };
  };
};

export const pushAll = function (as) {
  return function (xs) {
    return function () {
      return xs.push.apply(xs, as);
    };
  };
};

export const shiftImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function () {
        return xs.length > 0 ? just(xs.shift()) : nothing;
      };
    };
  };
};

export const unshiftAll = function (as) {
  return function (xs) {
    return function () {
      return xs.unshift.apply(xs, as);
    };
  };
};

export const splice = function (i) {
  return function (howMany) {
    return function (bs) {
      return function (xs) {
        return function () {
          return xs.splice.apply(xs, [i, howMany].concat(bs));
        };
      };
    };
  };
};

export const unsafeFreeze = function (xs) {
  return function () {
    return xs;
  };
};

export const unsafeThaw = function (xs) {
  return function () {
    return xs;
  };
};

function copyImpl(xs) {
  return function () {
    return xs.slice();
  };
}

export const freeze = copyImpl;

export const thaw = copyImpl;

export const sortByImpl = (function () {
  function mergeFromTo(compare, fromOrdering, xs1, xs2, from, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;

    mid = from + ((to - from) >> 1);
    if (mid - from > 1) mergeFromTo(compare, fromOrdering, xs2, xs1, from, mid);
    if (to - mid > 1) mergeFromTo(compare, fromOrdering, xs2, xs1, mid, to);

    i = from;
    j = mid;
    k = from;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      }
      else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }

  return function (compare) {
    return function (fromOrdering) {
      return function (xs) {
        return function () {
          if (xs.length < 2) return xs;

          mergeFromTo(compare, fromOrdering, xs, xs.slice(0), 0, xs.length);

          return xs;
        };
      };
    };
  };
})();

export const toAssocArray = function (xs) {
  return function () {
    var n = xs.length;
    var as = new Array(n);
    for (var i = 0; i < n; i++) as[i] = { value: xs[i], index: i };
    return as;
  };
};
