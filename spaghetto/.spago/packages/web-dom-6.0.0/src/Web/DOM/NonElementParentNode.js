export function _getElementById(id) {
  return function (node) {
    return function () {
      return node.getElementById(id);
    };
  };
}
