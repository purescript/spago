var getProp = function (name) {
  return function (doctype) {
    return doctype[name];
  };
};

export const _namespaceURI = getProp("namespaceURI");
export const _prefix = getProp("prefix");
export const localName = getProp("localName");
export const tagName = getProp("tagName");

export function id(node) {
  return function () {
    return node.id;
  };
}

export function setId(id) {
  return function (node) {
    return function () {
      node.id = id;
    };
  };
}

export function className(node) {
  return function () {
    return node.className;
  };
}

export function classList(element) {
  return function () {
    return element.classList;
  };
}

export function setClassName(className) {
  return function (node) {
    return function () {
      node.className = className;
    };
  };
}

export function getElementsByTagName(localName) {
  return function (doc) {
    return function () {
      return doc.getElementsByTagName(localName);
    };
  };
}

export function _getElementsByTagNameNS(ns) {
  return function (localName) {
    return function (doc) {
      return function () {
        return doc.getElementsByTagNameNS(ns, localName);
      };
    };
  };
}

export function getElementsByClassName(classNames) {
  return function (doc) {
    return function () {
      return doc.getElementsByClassName(classNames);
    };
  };
}

export function setAttribute(name) {
  return function (value) {
    return function (element) {
      return function () {
        element.setAttribute(name, value);
      };
    };
  };
}

export function _getAttribute(name) {
  return function (element) {
    return function () {
      return element.getAttribute(name);
    };
  };
}

export function hasAttribute(name) {
  return function (element) {
    return function () {
      return element.hasAttribute(name);
    };
  };
}

export function removeAttribute(name) {
  return function (element) {
    return function () {
      element.removeAttribute(name);
    };
  };
}

export function matches(selector) {
  return function(element) {
    return function () {
      return element.matches(selector);
    };
  };
}

export function _closest(selector) {
  return function(element) {
    return function () {
      return element.closest(selector);
    };
  };
}

// - CSSOM ---------------------------------------------------------------------

export function scrollTop(node) {
  return function () {
    return node.scrollTop;
  };
}

export function setScrollTop(scrollTop) {
  return function (node) {
    return function () {
      node.scrollTop = scrollTop;
    };
  };
}

export function scrollLeft(node) {
  return function () {
    return node.scrollLeft;
  };
}

export function setScrollLeft(scrollLeft) {
  return function (node) {
    return function () {
      node.scrollLeft = scrollLeft;
    };
  };
}

export function scrollWidth(el) {
  return function () {
    return el.scrollWidth;
  };
}

export function scrollHeight(el) {
  return function () {
    return el.scrollHeight;
  };
}

export function clientTop(el) {
  return function () {
    return el.clientTop;
  };
}

export function clientLeft(el) {
  return function () {
    return el.clientLeft;
  };
}

export function clientWidth(el) {
  return function () {
    return el.clientWidth;
  };
}

export function clientHeight(el) {
  return function () {
    return el.clientHeight;
  };
}

export function getBoundingClientRect(el) {
  return function () {
    var rect = el.getBoundingClientRect();
    return {
      top: rect.top,
      right: rect.right,
      bottom: rect.bottom,
      left: rect.left,
      width: rect.width,
      height: rect.height,
      x: rect.x,
      y: rect.y
    };
  };
}

export function _attachShadow(props) {
  return function (el) {
    return function() {
      return el.attachShadow(props);
    };
  };
}
