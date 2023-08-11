export function typeString(mr) {
  return function () {
    return mr.type;
  };
}

export function target(mr) {
  return function () {
    return mr.target;
  };
}

export function addedNodes(mr) {
  return function () {
    return mr.addedNodes;
  };
}

export function removedNodes(mr) {
  return function () {
    return mr.removedNodes;
  };
}

export function _nextSibling(mr) {
  return function () {
    return mr.nextSibling;
  };
}

export function _previousSibling(mr) {
  return function () {
    return mr.previousSibling;
  };
}

export function _attributeName(mr) {
  return function () {
    return mr.attributeName;
  };
}

export function _attributeNamespace(mr) {
  return function () {
    return mr.attributeNamespace;
  };
}

export function _oldValue(mr) {
  return function () {
    return mr.oldValue;
  };
}
