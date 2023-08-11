/* global Buffer */
export function byteLengthImpl(str) {
  return enc => {
    return Buffer.byteLength(str, enc);
  };
}
