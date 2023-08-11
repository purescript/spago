export function _mode(el) {
  return el.mode;
}

export function host(el) {
  return function() {
    return el.host;
  };
}
