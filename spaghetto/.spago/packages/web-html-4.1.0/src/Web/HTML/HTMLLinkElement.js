export function disabled(link) {
  return function () {
    return link.disabled;
  };
}

export function setDisabled(disabled) {
  return function (link) {
    return function () {
      link.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function href(link) {
  return function () {
    return link.href;
  };
}

export function setHref(href) {
  return function (link) {
    return function () {
      link.href = href;
    };
  };
}

// ----------------------------------------------------------------------------

export function crossOrigin(link) {
  return function () {
    return link.crossOrigin;
  };
}

export function setCrossOrigin(crossOrigin) {
  return function (link) {
    return function () {
      link.crossOrigin = crossOrigin;
    };
  };
}

// ----------------------------------------------------------------------------

export function rel(link) {
  return function () {
    return link.rel;
  };
}

export function setRel(rel) {
  return function (link) {
    return function () {
      link.rel = rel;
    };
  };
}

// ----------------------------------------------------------------------------

export function rev(link) {
  return function () {
    return link.rev;
  };
}

export function setRev(rev) {
  return function (link) {
    return function () {
      link.rev = rev;
    };
  };
}

// ----------------------------------------------------------------------------

export function relList(link) {
  return function () {
    return link.relList;
  };
}

// ----------------------------------------------------------------------------

export function media(link) {
  return function () {
    return link.media;
  };
}

export function setMedia(media) {
  return function (link) {
    return function () {
      link.media = media;
    };
  };
}

// ----------------------------------------------------------------------------

export function hreflang(link) {
  return function () {
    return link.hreflang;
  };
}

export function setHreflang(hreflang) {
  return function (link) {
    return function () {
      link.hreflang = hreflang;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(link) {
  return function () {
    return link.type;
  };
}

export function setType(type) {
  return function (link) {
    return function () {
      link.type = type;
    };
  };
}
