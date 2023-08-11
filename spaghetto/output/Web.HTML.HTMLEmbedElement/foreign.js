export function src(embed) {
  return function () {
    return embed.src;
  };
}

export function setSrc(src) {
  return function (embed) {
    return function () {
      embed.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(embed) {
  return function () {
    return embed.type;
  };
}

export function setType(type) {
  return function (embed) {
    return function () {
      embed.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function width(embed) {
  return function () {
    return embed.width;
  };
}

export function setWidth(width) {
  return function (embed) {
    return function () {
      embed.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(embed) {
  return function () {
    return embed.height;
  };
}

export function setHeight(height) {
  return function (embed) {
    return function () {
      embed.height = height;
    };
  };
}
