export function value(progress) {
  return function () {
    return progress.value;
  };
}

export function setValue(value) {
  return function (progress) {
    return function () {
      progress.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function max(progress) {
  return function () {
    return progress.max;
  };
}

export function setMax(max) {
  return function (progress) {
    return function () {
      progress.max = max;
    };
  };
}

// ----------------------------------------------------------------------------

export function position(progress) {
  return function () {
    return progress.position;
  };
}

// ----------------------------------------------------------------------------

export function labels(progress) {
  return function () {
    return progress.labels;
  };
}
