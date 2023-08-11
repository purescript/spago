/* eslint-disable no-eq-null, eqeqeq */

const nullImpl = null;
export { nullImpl as null };

export function nullable(a, r, f) {
  return a == null ? r : f(a);
}

export function notNull(x) {
  return x;
}
