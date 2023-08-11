export function src(source) {
  return function () {
    return source.src;
  };
}

export function setSrc(src) {
  return function (source) {
    return function () {
      source.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(source) {
  return function () {
    return source.type;
  };
}

export function setType(type) {
  return function (source) {
    return function () {
      source.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function media(source) {
  return function () {
    return source.media;
  };
}

export function setMedia(media) {
  return function (source) {
    return function () {
      source.media = media;
    };
  };
}
