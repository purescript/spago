export function width(canvas) {
  return function () {
    return canvas.width;
  };
}

export function setWidth(width) {
  return function (canvas) {
    return function () {
      canvas.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(canvas) {
  return function () {
    return canvas.height;
  };
}

export function setHeight(height) {
  return function (canvas) {
    return function () {
      canvas.height = height;
    };
  };
}
