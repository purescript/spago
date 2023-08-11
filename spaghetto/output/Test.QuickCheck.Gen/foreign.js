/* global ArrayBuffer, Float32Array, Int32Array */
// module Test.QuickCheck.Gen

export function float32ToInt32(n) {
  var arr = new ArrayBuffer(4);
  var fv = new Float32Array(arr);
  var iv = new Int32Array(arr);
  fv[0] = n;
  return iv[0];
}
