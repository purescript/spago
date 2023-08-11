export function href(base) {
  return function () {
    return base.href;
  };
}

export function setHref(href) {
  return function (base) {
    return function () {
      base.href = href;
    };
  };
}

// ----------------------------------------------------------------------------

export function target(base) {
  return function () {
    return base.target;
  };
}

export function setTarget(target) {
  return function (base) {
    return function () {
      base.target = target;
    };
  };
}
