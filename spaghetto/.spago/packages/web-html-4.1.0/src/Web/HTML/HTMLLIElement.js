export function value(li) {
  return function () {
    return li.value;
  };
}

export function setValue(value) {
  return function (li) {
    return function () {
      li.value = value;
    };
  };
}
