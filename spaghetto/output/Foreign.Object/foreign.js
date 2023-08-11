export function _copyST(m) {
  return function () {
    var r = {};
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r[k] = m[k];
      }
    }
    return r;
  };
}

export const empty = {};

export function runST(f) {
  return f();
}

export function _fmapObject(m0, f) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      m[k] = f(m0[k]);
    }
  }
  return m;
}

export function _mapWithKey(m0, f) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      m[k] = f(k)(m0[k]);
    }
  }
  return m;
}

export function _foldM(bind) {
  return function (f) {
    return function (mz) {
      return function (m) {
        var acc = mz;
        function g(k) {
          return function (z) {
            return f(z)(k)(m[k]);
          };
        }
        for (var k in m) {
          if (hasOwnProperty.call(m, k)) {
            acc = bind(acc)(g(k));
          }
        }
        return acc;
      };
    };
  };
}

export function _foldSCObject(m, z, f, fromMaybe) {
  var acc = z;
  for (var k in m) {
    if (hasOwnProperty.call(m, k)) {
      var maybeR = f(acc)(k)(m[k]);
      var r = fromMaybe(null)(maybeR);
      if (r === null) return acc;
      else acc = r;
    }
  }
  return acc;
}

export function all(f) {
  return function (m) {
    for (var k in m) {
      if (hasOwnProperty.call(m, k) && !f(k)(m[k])) return false;
    }
    return true;
  };
}

export function size(m) {
  var s = 0;
  for (var k in m) {
    if (hasOwnProperty.call(m, k)) {
      ++s;
    }
  }
  return s;
}

export function _lookup(no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
}

export function _lookupST(no, yes, k, m) {
  return function () {
    return k in m ? yes(m[k]) : no;
  };
}

export function toArrayWithKey(f) {
  return function (m) {
    var r = [];
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
}

export const keys = Object.keys || toArrayWithKey(function (k) {
  return function () { return k; };
});
