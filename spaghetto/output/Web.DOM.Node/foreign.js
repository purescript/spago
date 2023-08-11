var getEffProp = function (name) {
  return function (node) {
    return function () {
      return node[name];
    };
  };
};

export function nodeTypeIndex(node) {
  return node.nodeType;
}

export function nodeName(node) {
  return node.nodeName;
}

export const baseURI = getEffProp("baseURI");
export const _ownerDocument = getEffProp("ownerDocument");
export const _parentNode = getEffProp("parentNode");
export const _parentElement = getEffProp("parentElement");

export function hasChildNodes(node) {
  return function () {
    return node.hasChildNodes();
  };
}

export const childNodes = getEffProp("childNodes");
export const _firstChild = getEffProp("firstChild");
export const _lastChild = getEffProp("lastChild");
export const _previousSibling = getEffProp("previousSibling");
export const _nextSibling = getEffProp("nextSibling");
export const _nodeValue = getEffProp("nodeValue");

export function setNodeValue(value) {
  return function (node) {
    return function () {
      node.nodeValue = value;
    };
  };
}

export const textContent = getEffProp("textContent");

export function setTextContent(value) {
  return function (node) {
    return function () {
      node.textContent = value;
    };
  };
}

export function normalize(node) {
  return function () {
    node.normalize();
  };
}

export function clone(node) {
  return function () {
    return node.cloneNode(false);
  };
}

export function deepClone(node) {
  return function () {
    return node.cloneNode(true);
  };
}

export function isEqualNode(node1) {
  return function (node2) {
    return function () {
      return node1.isEqualNode(node2);
    };
  };
}

export function compareDocumentPositionBits(node1) {
  return function (node2) {
    return function () {
      return node1.compareDocumentPosition(node2);
    };
  };
}

export function contains(node1) {
  return function (node2) {
    return function () {
      return node1.contains(node2);
    };
  };
}

export function _lookupPrefix(prefix) {
  return function (node) {
    return function () {
      return node.lookupPrefix(prefix);
    };
  };
}

export function _lookupNamespaceURI(ns) {
  return function (node) {
    return function () {
      return node.lookupNamespaceURI(ns);
    };
  };
}

export function isDefaultNamespace(ns) {
  return function (node) {
    return function () {
      return node.isDefaultNamespace(ns);
    };
  };
}

export function insertBefore(node1) {
  return function (node2) {
    return function (parent) {
      return function () {
        parent.insertBefore(node1, node2);
      };
    };
  };
}

export function appendChild(node) {
  return function (parent) {
    return function () {
      parent.appendChild(node);
    };
  };
}

export function replaceChild(newChild) {
  return function (oldChild) {
    return function (parent) {
      return function () {
        parent.replaceChild(newChild, oldChild);
      };
    };
  };
}

export function removeChild(node) {
  return function (parent) {
    return function () {
      parent.removeChild(node);
    };
  };
}
