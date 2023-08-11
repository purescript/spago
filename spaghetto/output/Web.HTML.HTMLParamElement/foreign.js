export function name(param) {
  return function () {
    return param.name;
  };
}

export function setName(name) {
  return function (param) {
    return function () {
      param.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(param) {
  return function () {
    return param.value;
  };
}

export function setValue(value) {
  return function (param) {
    return function () {
      param.value = value;
    };
  };
}
