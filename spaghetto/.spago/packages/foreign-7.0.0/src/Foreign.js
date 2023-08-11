export function typeOf(value) {
  return typeof value;
}

export function tagOf(value) {
  return Object.prototype.toString.call(value).slice(8, -1);
}

export function isNull(value) {
  return value === null;
}

export function isUndefined(value) {
  return value === undefined;
}

export const isArray = Array.isArray || function (value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};
