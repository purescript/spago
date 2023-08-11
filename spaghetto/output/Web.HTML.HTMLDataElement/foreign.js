export function value(data) {
  return function () {
    return data.value;
  };
}

export function setValue(value) {
  return function (data) {
    return function () {
      data.value = value;
    };
  };
}
