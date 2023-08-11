export function returnValue(e) {
  return function () {
    return e.returnValue;
  };
}

export function setReturnValue(v) {
  return function (e) {
    return function () {
      e.returnValue = v;
    };
  };
}
