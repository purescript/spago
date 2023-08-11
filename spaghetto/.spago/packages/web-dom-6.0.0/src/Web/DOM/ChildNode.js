export function remove(node) {
  return function () {
    return node.remove();
  };
}
