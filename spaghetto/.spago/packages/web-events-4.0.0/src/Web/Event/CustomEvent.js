const newSimple = function(ty) {
  return function () {
    return new CustomEvent(ty);
  };
};
export { newSimple as new };

export function newOptionsImpl (ty) {
  return function (opt) {
    return function () {
      return new CustomEvent(ty, opt);
    };
  };
}

export function detail(e) {
  return e.detail;
}
