export function unsafeUnionFn(r1, r2) {
  var copy = {};
  for (var k1 in r2) {
    if ({}.hasOwnProperty.call(r2, k1)) {
      copy[k1] = r2[k1];
    }
  }
  for (var k2 in r1) {
    if ({}.hasOwnProperty.call(r1, k2)) {
      copy[k2] = r1[k2];
    }
  }
  return copy;
}
