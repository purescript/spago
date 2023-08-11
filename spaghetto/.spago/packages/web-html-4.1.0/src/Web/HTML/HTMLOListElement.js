export function reversed(ol) {
  return function () {
    return ol.reversed;
  };
}

export function setReversed(reversed) {
  return function (ol) {
    return function () {
      ol.reversed = reversed;
    };
  };
}

// ----------------------------------------------------------------------------

export function start(ol) {
  return function () {
    return ol.start;
  };
}

export function setStart(start) {
  return function (ol) {
    return function () {
      ol.start = start;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(ol) {
  return function () {
    return ol.type;
  };
}

export function setType(type) {
  return function (ol) {
    return function () {
      ol.type = type;
    };
  };
}
