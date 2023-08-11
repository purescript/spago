function wrap(method) {
  return function(d) {
    return function(num) {
      return method.apply(num, [d]);
    };
  };
}

export const toPrecisionNative = wrap(Number.prototype.toPrecision);
export const toFixedNative = wrap(Number.prototype.toFixed);
export const toExponentialNative = wrap(Number.prototype.toExponential);
export function toString(num) { return num.toString(); }
