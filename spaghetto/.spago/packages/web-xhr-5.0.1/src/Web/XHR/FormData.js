const newImpl = function () {
  return new FormData();
};
export { newImpl as new };

export function _fromFormElement(form) {
  return new FormData(form);
}

export function _append(name, value, fd) {
  fd.append(name, value);
}

export function _appendBlob(name, value, filename, fd) {
  fd.append(name, value, filename === null ? undefined : filename);
}

export function _delete(name, fd) {
  fd.delete(name);
}

export function _has(name, fd) {
  return fd.has(name);
}

export function _set(name, value, fd) {
  fd.set(name, value);
}

export function _setBlob(name, value, filename, fd) {
  fd.set(name, value, filename === null ? undefined : filename);
}
