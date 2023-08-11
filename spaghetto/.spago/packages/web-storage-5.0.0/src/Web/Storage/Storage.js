export function length(storage) {
  return function () {
    return storage.length;
  };
}

export function _key(index) {
  return function (storage) {
    return function () {
      return storage.key(index);
    };
  };
}

export function _getItem(key) {
  return function (storage) {
    return function () {
      return storage.getItem(key);
    };
  };
}

export function setItem(key) {
  return function (value) {
    return function (storage) {
      return function () {
        storage.setItem(key, value);
      };
    };
  };
}

export function removeItem(key) {
  return function (storage) {
    return function () {
      storage.removeItem(key);
    };
  };
}

export function clear(storage) {
  return function () {
    storage.clear();
  };
}
