export function src(script) {
  return function () {
    return script.src;
  };
}

export function setSrc(src) {
  return function (script) {
    return function () {
      script.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(script) {
  return function () {
    return script.type;
  };
}

export function setType(type) {
  return function (script) {
    return function () {
      script.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function charset(script) {
  return function () {
    return script.charset;
  };
}

export function setCharset(charset) {
  return function (script) {
    return function () {
      script.charset = charset;
    };
  };
}

// ----------------------------------------------------------------------------

export function async(script) {
  return function () {
    return script.async;
  };
}

export function setAsync(async) {
  return function (script) {
    return function () {
      script.async = async;
    };
  };
}

// ----------------------------------------------------------------------------

export function defer(script) {
  return function () {
    return script.defer;
  };
}

export function setDefer(defer) {
  return function (script) {
    return function () {
      script.defer = defer;
    };
  };
}

// ----------------------------------------------------------------------------

export function crossOrigin(script) {
  return function () {
    return script.crossOrigin;
  };
}

export function setCrossOrigin(crossOrigin) {
  return function (script) {
    return function () {
      script.crossOrigin = crossOrigin;
    };
  };
}

// ----------------------------------------------------------------------------

export function text(script) {
  return function () {
    return script.text;
  };
}

export function setText(text) {
  return function (script) {
    return function () {
      script.text = text;
    };
  };
}
