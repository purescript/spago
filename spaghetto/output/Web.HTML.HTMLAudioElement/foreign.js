export function create() {
  return new Audio();
}

export function createWithURL(url) {
  return function () {
    return new Audio(url);
  };
}
