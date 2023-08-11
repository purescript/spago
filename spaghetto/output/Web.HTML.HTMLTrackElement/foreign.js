export function kind(track) {
  return function () {
    return track.kind;
  };
}

export function setKind(kind) {
  return function (track) {
    return function () {
      track.kind = kind;
    };
  };
}

// ----------------------------------------------------------------------------

export function src(track) {
  return function () {
    return track.src;
  };
}

export function setSrc(src) {
  return function (track) {
    return function () {
      track.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function srclang(track) {
  return function () {
    return track.srclang;
  };
}

export function setSrclang(srclang) {
  return function (track) {
    return function () {
      track.srclang = srclang;
    };
  };
}

// ----------------------------------------------------------------------------

export function label(track) {
  return function () {
    return track.label;
  };
}

export function setLabel(label) {
  return function (track) {
    return function () {
      track.label = label;
    };
  };
}

// ----------------------------------------------------------------------------

const defaultImpl = function (track) {
  return function () {
    return track["default"];
  };
};
export { defaultImpl as default };

export function setDefault(def) {
  return function (track) {
    return function () {
      track["default"] = def;
    };
  };
}

// ----------------------------------------------------------------------------

export function _readyState(track) {
  return track.readyState;
}
