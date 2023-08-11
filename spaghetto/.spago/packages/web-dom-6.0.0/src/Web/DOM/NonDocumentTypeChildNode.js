export function _previousElementSibling(node) {
  return function () {
    return node.previousElementSibling;
  };
}

export function _nextElementSibling(node) {
  return function () {
    return node.nextElementSibling;
  };
}
