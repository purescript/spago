export function reallyUnsafeRefEq(a) {
  return function (b) {
    return a === b;
  };
}
