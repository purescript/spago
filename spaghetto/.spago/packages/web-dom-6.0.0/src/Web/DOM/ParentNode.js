var getEffProp = function (name) {
  return function (node) {
    return function () {
      return node[name];
    };
  };
};

export const children = getEffProp("children");
export const _firstElementChild = getEffProp("firstElementChild");
export const _lastElementChild = getEffProp("lastElementChild");
export const childElementCount = getEffProp("childElementCount");

export function _querySelector(selector) {
  return function (node) {
    return function () {
      return node.querySelector(selector);
    };
  };
}

export function querySelectorAll(selector) {
  return function (node) {
    return function () {
      return node.querySelectorAll(selector);
    };
  };
}
