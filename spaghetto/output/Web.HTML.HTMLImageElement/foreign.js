export function create() {
  return new Image();
}

export function createWithDimensions(width) {
  return function (height) {
    return function () {
      return new Image(width, height);
    };
  };
}

// ----------------------------------------------------------------------------

export function alt(image) {
  return function () {
    return image.alt;
  };
}

export function setAlt(alt) {
  return function (image) {
    return function () {
      image.alt = alt;
    };
  };
}

// ----------------------------------------------------------------------------

export function src(image) {
  return function () {
    return image.src;
  };
}

export function setSrc(src) {
  return function (image) {
    return function () {
      image.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function srcset(image) {
  return function () {
    return image.srcset;
  };
}

export function setSrcset(srcset) {
  return function (image) {
    return function () {
      image.srcset = srcset;
    };
  };
}

// ----------------------------------------------------------------------------

export function sizes(image) {
  return function () {
    return image.sizes;
  };
}

export function setSizes(sizes) {
  return function (image) {
    return function () {
      image.sizes = sizes;
    };
  };
}

// ----------------------------------------------------------------------------

export function currentSrc(image) {
  return function () {
    return image.currentSrc;
  };
}

// ----------------------------------------------------------------------------

export function _crossOrigin(image) {
  return image.crossOrigin;
}

export function _setCrossOrigin(crossOrigin, image) {
  image.crossOrigin = crossOrigin;
}

// ----------------------------------------------------------------------------

export function useMap(image) {
  return function () {
    return image.useMap;
  };
}

export function setUseMap(useMap) {
  return function (image) {
    return function () {
      image.useMap = useMap;
    };
  };
}

// ----------------------------------------------------------------------------

export function isMap(image) {
  return function () {
    return image.isMap;
  };
}

export function setIsMap(isMap) {
  return function (image) {
    return function () {
      image.isMap = isMap;
    };
  };
}

// ----------------------------------------------------------------------------

export function width(image) {
  return function () {
    return image.width;
  };
}

export function setWidth(width) {
  return function (image) {
    return function () {
      image.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(image) {
  return function () {
    return image.height;
  };
}

export function setHeight(height) {
  return function (image) {
    return function () {
      image.height = height;
    };
  };
}

// ----------------------------------------------------------------------------

export function naturalWidth(image) {
  return function () {
    return image.naturalWidth;
  };
}

export function naturalHeight(image) {
  return function () {
    return image.naturalHeight;
  };
}

// ----------------------------------------------------------------------------

export function referrerPolicy(image) {
  return function () {
    return image.referrerPolicy;
  };
}

export function setReferrerPolicy(referrerPolicy) {
  return function (image) {
    return function () {
      image.referrerPolicy = referrerPolicy;
    };
  };
}

// ----------------------------------------------------------------------------

export function _decoding(image) {
  return image.decoding;
}

export function _setDecoding(decoding, image) {
  image.decoding = decoding;
}

// ----------------------------------------------------------------------------

export function _loading(image) {
  return image.loading;
}

export function _setLoading(loading, image) {
  image.loading = loading;
}

// ----------------------------------------------------------------------------

export function complete(image) {
  return function () {
    return image.complete;
  };
}
