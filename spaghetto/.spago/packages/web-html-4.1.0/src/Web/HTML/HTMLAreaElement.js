export function alt(area) {
  return function () {
    return area.alt;
  };
}

export function setAlt(alt) {
  return function (area) {
    return function () {
      area.alt = alt;
    };
  };
}

// ----------------------------------------------------------------------------

export function coords(area) {
  return function () {
    return area.coords;
  };
}

export function setCoords(coords) {
  return function (area) {
    return function () {
      area.coords = coords;
    };
  };
}

// ----------------------------------------------------------------------------

export function shape(area) {
  return function () {
    return area.shape;
  };
}

export function setShape(shape) {
  return function (area) {
    return function () {
      area.shape = shape;
    };
  };
}

// ----------------------------------------------------------------------------

export function target(area) {
  return function () {
    return area.target;
  };
}

export function setTarget(target) {
  return function (area) {
    return function () {
      area.target = target;
    };
  };
}

// ----------------------------------------------------------------------------

export function download(area) {
  return function () {
    return area.download;
  };
}

export function setDownload(download) {
  return function (area) {
    return function () {
      area.download = download;
    };
  };
}

// ----------------------------------------------------------------------------

export function rel(area) {
  return function () {
    return area.rel;
  };
}

export function setRel(rel) {
  return function (area) {
    return function () {
      area.rel = rel;
    };
  };
}

// ----------------------------------------------------------------------------

export function relList(area) {
  return function () {
    return area.relList;
  };
}

// ----------------------------------------------------------------------------

export function hreflang(area) {
  return function () {
    return area.hreflang;
  };
}

export function setHreflang(hreflang) {
  return function (area) {
    return function () {
      area.hreflang = hreflang;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(area) {
  return function () {
    return area.type;
  };
}

export function setType(type) {
  return function (area) {
    return function () {
      area.type = type;
    };
  };
}
