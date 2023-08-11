export function splitText(offset) {
  return function (t) {
    return function () {
      return t.splitText(offset);
    };
  };
}

export function wholeText(t) {
  return function () {
    return t.wholeText;
  };
}
