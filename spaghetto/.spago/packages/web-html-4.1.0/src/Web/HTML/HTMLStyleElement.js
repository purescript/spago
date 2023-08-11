export function media(style) {
  return function () {
    return style.media;
  };
}

export function setMedia(media) {
  return function (style) {
    return function () {
      style.media = media;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(style) {
  return function () {
    return style.type;
  };
}

export function setType(type) {
  return function (style) {
    return function () {
      style.type = type;
    };
  };
}
