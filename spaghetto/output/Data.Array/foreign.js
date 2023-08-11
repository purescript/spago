//------------------------------------------------------------------------------
// Array creation --------------------------------------------------------------
//------------------------------------------------------------------------------

export const range = function (start) {
  return function (end) {
    var step = start > end ? -1 : 1;
    var result = new Array(step * (end - start) + 1);
    var i = start, n = 0;
    while (i !== end) {
      result[n++] = i;
      i += step;
    }
    result[n] = i;
    return result;
  };
};

var replicateFill = function (count) {
  return function (value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
};

var replicatePolyfill = function (count) {
  return function (value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
};

// In browsers that have Array.prototype.fill we use it, as it's faster.
export const replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;

export const fromFoldableImpl = (function () {
  function Cons(head, tail) {
    this.head = head;
    this.tail = tail;
  }
  var emptyList = {};

  function curryCons(head) {
    return function (tail) {
      return new Cons(head, tail);
    };
  }

  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }

  return function (foldr) {
    return function (xs) {
      return listToArray(foldr(curryCons)(emptyList)(xs));
    };
  };
})();

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

export const length = function (xs) {
  return xs.length;
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

export const unconsImpl = function (empty) {
  return function (next) {
    return function (xs) {
      return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
    };
  };
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

export const indexImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
      };
    };
  };
};

export const findMapImpl = function (nothing) {
  return function (isJust) {
    return function (f) {
      return function (xs) {
        for (var i = 0; i < xs.length; i++) {
          var result = f(xs[i]);
          if (isJust(result)) return result;
        }
        return nothing;
      };
    };
  };
};

export const findIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

export const findLastIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = xs.length - 1; i >= 0; i--) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

export const _insertAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i > l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 0, a);
          return just(l1);
        };
      };
    };
  };
};

export const _deleteAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (l) {
        if (i < 0 || i >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i, 1);
        return just(l1);
      };
    };
  };
};

export const _updateAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1[i] = a;
          return just(l1);
        };
      };
    };
  };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

export const reverse = function (l) {
  return l.slice().reverse();
};

export const concat = function (xss) {
  if (xss.length <= 10000) {
    // This method is faster, but it crashes on big arrays.
    // So we use it when can and fallback to simple variant otherwise.
    return Array.prototype.concat.apply([], xss);
  }

  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

export const filter = function (f) {
  return function (xs) {
    return xs.filter(f);
  };
};

export const partition = function (f) {
  return function (xs) {
    var yes = [];
    var no  = [];
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (f(x))
        yes.push(x);
      else
        no.push(x);
    }
    return { yes: yes, no: no };
  };
};

export const scanl = function (f) {
  return function (b) {
    return function (xs) {
      var len = xs.length;
      var acc = b;
      var out = new Array(len);
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
        out[i] = acc;
      }
      return out;
    };
  };
};

export const scanr = function (f) {
  return function (b) {
    return function (xs) {
      var len = xs.length;
      var acc = b;
      var out = new Array(len);
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
        out[i] = acc;
      }
      return out;
    };
  };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

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
        var out;

        if (xs.length < 2) return xs;

        out = xs.slice(0);
        mergeFromTo(compare, fromOrdering, out, xs.slice(0), 0, xs.length);

        return out;
      };
    };
  };
})();

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

export const slice = function (s) {
  return function (e) {
    return function (l) {
      return l.slice(s, e);
    };
  };
};

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

export const zipWith = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

//------------------------------------------------------------------------------
// Folding ---------------------------------------------------------------------
//------------------------------------------------------------------------------

export const any = function (p) {
  return function (xs) {
    var len = xs.length;
    for (var i = 0; i < len; i++) {
      if (p(xs[i])) return true;
    }
    return false;
  };
};

export const all = function (p) {
  return function (xs) {
    var len = xs.length;
    for (var i = 0; i < len; i++) {
      if (!p(xs[i])) return false;
    }
    return true;
  };
};

//------------------------------------------------------------------------------
// Partial ---------------------------------------------------------------------
//------------------------------------------------------------------------------

export const unsafeIndexImpl = function (xs) {
  return function (n) {
    return xs[n];
  };
};
