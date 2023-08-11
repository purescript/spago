export function target(a) {
  return function () {
    return a.target;
  };
}

export function setTarget(target) {
  return function (a) {
    return function () {
      a.target = target;
    };
  };
}

// ----------------------------------------------------------------------------

export function download(a) {
  return function () {
    return a.download;
  };
}

export function setDownload(download) {
  return function (a) {
    return function () {
      a.download = download;
    };
  };
}

// ----------------------------------------------------------------------------

export function rel(a) {
  return function () {
    return a.rel;
  };
}

export function setRel(rel) {
  return function (a) {
    return function () {
      a.rel = rel;
    };
  };
}

// ----------------------------------------------------------------------------

export function rev(a) {
  return function () {
    return a.rev;
  };
}

export function setRev(rev) {
  return function (a) {
    return function () {
      a.rev = rev;
    };
  };
}

// ----------------------------------------------------------------------------

export function relList(a) {
  return function () {
    return a.relList;
  };
}

// ----------------------------------------------------------------------------

export function hreflang(a) {
  return function () {
    return a.hreflang;
  };
}

export function setHreflang(hreflang) {
  return function (a) {
    return function () {
      a.hreflang = hreflang;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(a) {
  return function () {
    return a.type;
  };
}

export function setType(type) {
  return function (a) {
    return function () {
      a.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function text(a) {
  return function () {
    return a.text;
  };
}

export function setText(text) {
  return function (a) {
    return function () {
      a.text = text;
    };
  };
}
