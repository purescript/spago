export function back(history) {
  return function() {
    return history.back();
  };
}

export function forward(history) {
  return function() {
    return history.forward();
  };
}

export function go(delta) {
  return function(history) {
    return function() {
      return history.go(delta);
    };
  };
}

export function pushState(a) {
  return function(docTitle) {
    return function(url) {
      return function(history) {
        return function() {
          return history.pushState(a, docTitle, url);
        };
      };
    };
  };
}

export function replaceState(a) {
  return function(docTitle) {
    return function(url) {
      return function(history) {
        return function() {
          return history.replaceState(a, docTitle, url);
        };
      };
    };
  };
}

export function state(history) {
  return function() {
    return history.state;
  };
}
