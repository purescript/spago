export function unsafeIndex(m) {
  return function (k) {
    return m[k];
  };
}
