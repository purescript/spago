export function now() {
  return Date.now();
}

export function getTimezoneOffset() {
  var n = new Date(Date.now());
  return n.getTimezoneOffset();
}
