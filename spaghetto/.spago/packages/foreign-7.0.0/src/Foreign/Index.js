export function unsafeReadPropImpl(f, s, key, value) {
  return value == null ? f : s(value[key]);
}

export function unsafeHasOwnProperty(prop, value) {
  return Object.prototype.hasOwnProperty.call(value, prop);
}

export function unsafeHasProperty(prop, value) {
  return prop in value;
}
