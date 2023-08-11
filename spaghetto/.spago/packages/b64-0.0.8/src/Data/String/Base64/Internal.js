export const atobIsDefined = typeof atob === "function";
export const btoaIsDefined = typeof btoa === "function";

// This function converts a `Uint8Array` to a btoa-safe string.
// It does so by treating each byte as a Unicode code point value and by
// concatenating the corresponding characters.
// This means that e.g. a three-byte UTF-8 character is mapped to three
// different characters with code points between 0 .. U+00FF.
// This is also the reason why `String.fromCharCode` is perfectly safe here.
export function uint8ArrayToBtoaSafeStringImpl (u8) {
  const chunkSize = 0x8000; // Chunk size used for reading large arrays
  let cs = [];

  for (let i = 0; i < u8.length; i += chunkSize) {
    cs.push(String.fromCharCode.apply(null, u8.subarray(i, i + chunkSize)));
  }

  return cs.join("");
};

// Inspired by `purescript-typedarray`. Unfortunately, the future of that
// library is currently (2018-07-18) uncertain.
export function asUint8ArrayImpl (array) {
  return new Uint8Array(array);
}
