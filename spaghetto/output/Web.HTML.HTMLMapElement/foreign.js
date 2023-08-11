export function name(map) {
  return function () {
    return map.name;
  };
}

export function setName(name) {
  return function (map) {
    return function () {
      map.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function areas(map) {
  return function () {
    return map.areas;
  };
}

// ----------------------------------------------------------------------------

export function images(map) {
  return function () {
    return map.images;
  };
}
