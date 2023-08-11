export function promise(f) {
  return function () {
    return new Promise(function (success, error) {
      var succF = function (s) { return function() { return success(s); } };
      var failF = function (s) { return function() { return error(s); } };

      // This indicates the aff was wrong?
      try { f(succF)(failF)(); }
      catch (e) {
        error(e);
      }
    });
  };
}

export function thenImpl(promise) {
  return function(errCB) {
    return function(succCB) {
      return function() {
        promise.then(succCB, errCB);
      };
    };
  };
}
