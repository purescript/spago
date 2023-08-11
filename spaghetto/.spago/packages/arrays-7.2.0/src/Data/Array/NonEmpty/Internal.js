export const foldr1Impl = function (f) {
  return function (xs) {
    var acc = xs[xs.length - 1];
    for (var i = xs.length - 2; i >= 0; i--) {
      acc = f(xs[i])(acc);
    }
    return acc;
  };
};

export const foldl1Impl = function (f) {
  return function (xs) {
    var acc = xs[0];
    var len = xs.length;
    for (var i = 1; i < len; i++) {
      acc = f(acc)(xs[i]);
    }
    return acc;
  };
};

export const traverse1Impl = function () {
  function Cont(fn) {
    this.fn = fn;
  }

  var emptyList = {};

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function finalCell(head) {
    return new ConsCell(head, emptyList);
  }

  function consList(x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (f) {
        var buildFrom = function (x, ys) {
          return apply(map(consList)(f(x)))(ys);
        };

        var go = function (acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last = xs[currentLen - 1];
            return new Cont(function () {
              var built = go(buildFrom(last, acc), currentLen - 1, xs);
              return built;
            });
          }
        };

        return function (array) {
          var acc = map(finalCell)(f(array[array.length - 1]));
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }

          return map(listToArray)(result);
        };
      };
    };
  };
}();
