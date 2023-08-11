export function createObjectURL(blob) {
  return function () {
    return URL.createObjectURL(blob);
  };
}

export function revokeObjectURL(url) {
  return function () {
    URL.revokeObjectURL(url);
  };
}
