export function src(iframe) {
  return function () {
    return iframe.src;
  };
}

export function setSrc(src) {
  return function (iframe) {
    return function () {
      iframe.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function srcdoc(iframe) {
  return function () {
    return iframe.srcdoc;
  };
}

export function setSrcdoc(srcdoc) {
  return function (iframe) {
    return function () {
      iframe.srcdoc = srcdoc;
    };
  };
}

// ----------------------------------------------------------------------------

export function name(iframe) {
  return function () {
    return iframe.name;
  };
}

export function setName(name) {
  return function (iframe) {
    return function () {
      iframe.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function width(iframe) {
  return function () {
    return iframe.width;
  };
}

export function setWidth(width) {
  return function (iframe) {
    return function () {
      iframe.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(iframe) {
  return function () {
    return iframe.height;
  };
}

export function setHeight(height) {
  return function (iframe) {
    return function () {
      iframe.height = height;
    };
  };
}

// ----------------------------------------------------------------------------

export function _contentDocument(iframe) {
  return function () {
    return iframe.contentDocument;
  };
}

export function _contentWindow(iframe) {
  return function () {
    return iframe.contentWindow;
  };
}
