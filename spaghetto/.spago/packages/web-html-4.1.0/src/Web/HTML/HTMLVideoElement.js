// ----------------------------------------------------------------------------

export function width(video) {
  return function () {
    return video.width;
  };
}

export function setWidth(width) {
  return function (video) {
    return function () {
      video.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(video) {
  return function () {
    return video.height;
  };
}

export function setHeight(height) {
  return function (video) {
    return function () {
      video.height = height;
    };
  };
}

// ----------------------------------------------------------------------------

export function videoWidth(video) {
  return function () {
    return video.videoWidth;
  };
}

// ----------------------------------------------------------------------------

export function videoHeight(video) {
  return function () {
    return video.videoHeight;
  };
}

// ----------------------------------------------------------------------------

export function poster(video) {
  return function () {
    return video.poster;
  };
}

export function setPoster(poster) {
  return function (video) {
    return function () {
      video.poster = poster;
    };
  };
}
