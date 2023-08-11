export function unsafeFreeze(m) {
  return function () {
    return m;
  };
}
