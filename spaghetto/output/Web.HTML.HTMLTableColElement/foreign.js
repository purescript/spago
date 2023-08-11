export function span(col) {
  return function () {
    return col.span;
  };
}

export function setSpan(span) {
  return function (col) {
    return function () {
      col.span = span;
    };
  };
}
