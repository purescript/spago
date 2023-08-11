export function document(window) {
  return function () {
    return window.document;
  };
}

export function navigator(window) {
  return function () {
    return window.navigator;
  };
}

export function location(window) {
  return function () {
    return window.location;
  };
}

export function history(window) {
  return function() {
    return window.history;
  };
}

export function innerWidth(window) {
  return function () {
    return window.innerWidth;
  };
}

export function innerHeight(window) {
  return function () {
    return window.innerHeight;
  };
}

export function alert(str) {
  return function (window) {
    return function () {
      window.alert(str);
    };
  };
}

export function confirm(str) {
  return function (window) {
    return function () {
      return window.confirm(str);
    };
  };
}

export function moveBy(xDelta) {
  return function (yDelta) {
    return function (window) {
      return function () {
        window.moveBy(xDelta, yDelta);
      };
    };
  };
}

export function moveTo(width) {
  return function (height) {
    return function (window) {
      return function () {
        window.moveTo(width, height);
      };
    };
  };
}

export function _open(url) {
  return function (name) {
    return function (features) {
      return function (window) {
        return function () {
          return window.open(url, name, features);
        };
      };
    };
  };
}

export function close(window) {
  return function () {
    return window.close();
  };
}

export function outerHeight(window) {
  return function () {
    return window.outerHeight;
  };
}

export function outerWidth(window) {
  return function () {
    return window.outerWidth;
  };
}

export function print(window) {
  return function () {
    window.print();
  };
}

export function _prompt(str) {
  return function (defaultText) {
    return function (window) {
      return function () {
        return window.prompt(str, defaultText);
      };
    };
  };
}

export function resizeBy(xDelta) {
  return function (yDelta) {
    return function (window) {
      return function () {
        window.resizeBy(xDelta, yDelta);
      };
    };
  };
}

export function resizeTo(width) {
  return function (height) {
    return function (window) {
      return function () {
        window.resizeTo(width, height);
      };
    };
  };
}

export function screenX(window) {
  return function () {
    return window.screenX;
  };
}

export function screenY(window) {
  return function () {
    return window.screenY;
  };
}

export function scroll(xCoord) {
  return function (yCoord) {
    return function (window) {
      return function () {
        window.scroll(xCoord, yCoord);
      };
    };
  };
}

export function scrollBy(xCoord) {
  return function (yCoord) {
    return function (window) {
      return function () {
        window.scrollBy(xCoord, yCoord);
      };
    };
  };
}

export function scrollX(window) {
  return function () {
    return window.scrollX;
  };
}

export function scrollY(window) {
  return function () {
    return window.scrollY;
  };
}

export function localStorage(window) {
  return function () {
    return window.localStorage;
  };
}

export function sessionStorage(window) {
  return function () {
    return window.sessionStorage;
  };
}

export function requestAnimationFrame(fn) {
  return function(window) {
    return function() {
      return window.requestAnimationFrame(fn);
    };
  };
}

export function cancelAnimationFrame(id) {
  return function(window) {
    return function() {
      return window.cancelAnimationFrame(id);
    };
  };
}

export function requestIdleCallback(opts) {
  return function(fn) {
    return function(window) {
      return function() {
        return window.requestIdleCallback(fn, opts);
      };
    };
  };
}

export function cancelIdleCallback(id) {
  return function(window) {
    return function() {
      return window.cancelIdleCallback(id);
    };
  };
}

export function parent(window) {
  return function() {
    return window.parent;
  };
}

export function _opener(window) {
  return function() {
    return window.opener;
  };
}
