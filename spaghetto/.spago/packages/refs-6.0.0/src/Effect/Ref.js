export const _new = function (val) {
  return function () {
    return { value: val };
  };
};

export const newWithSelf = function (f) {
  return function () {
    var ref = { value: null };
    ref.value = f(ref);
    return ref;
  };
};

export const read = function (ref) {
  return function () {
    return ref.value;
  };
};

export const modifyImpl = function (f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

export const write = function (val) {
  return function (ref) {
    return function () {
      ref.value = val;
    };
  };
};
