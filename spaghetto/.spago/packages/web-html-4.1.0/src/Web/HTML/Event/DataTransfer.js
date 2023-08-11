export function _files(dataTransfer) {
  return dataTransfer.files;
}

export function items(dataTransfer) {
  return dataTransfer.items;
}

export function types(dataTransfer) {
  return dataTransfer.types;
}

export function _getData(format) {
  return function (dataTransfer) {
    return function () {
      return dataTransfer.getData(format);
    };
  };
}

export function _setData(format) {
  return function (data) {
    return function (dataTransfer) {
      return function () {
        return dataTransfer.setData(format, data);
      };
    };
  };
}

export function _setDragImage(dataTransfer) {
  return function (image) {
    return function (x) {
      return function (y) {
        return function () {
          return dataTransfer.setDragImage(image, x, y);
        };
      };
    };
  };
}

export function _dropEffect(dataTransfer) {
  return function () {
    return dataTransfer.dropEffect;
  };
}

export function _setDropEffect(e) {
  return function (dataTransfer) {
    return function () {
      dataTransfer.dropEffect = e;
    };
  };
}
