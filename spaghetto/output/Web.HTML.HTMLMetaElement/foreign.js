export function name(meta) {
  return function () {
    return meta.name;
  };
}

export function setName(name) {
  return function (meta) {
    return function () {
      meta.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function httpEquiv(meta) {
  return function () {
    return meta.httpEquiv;
  };
}

export function setHttpEquiv(httpEquiv) {
  return function (meta) {
    return function () {
      meta.httpEquiv = httpEquiv;
    };
  };
}

// ----------------------------------------------------------------------------

export function content(meta) {
  return function () {
    return meta.content;
  };
}

export function setContent(content) {
  return function (meta) {
    return function () {
      meta.content = content;
    };
  };
}
