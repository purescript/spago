export function value(meter) {
  return function () {
    return meter.value;
  };
}

export function setValue(value) {
  return function (meter) {
    return function () {
      meter.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function min(meter) {
  return function () {
    return meter.min;
  };
}

export function setMin(min) {
  return function (meter) {
    return function () {
      meter.min = min;
    };
  };
}

// ----------------------------------------------------------------------------

export function max(meter) {
  return function () {
    return meter.max;
  };
}

export function setMax(max) {
  return function (meter) {
    return function () {
      meter.max = max;
    };
  };
}

// ----------------------------------------------------------------------------

export function low(meter) {
  return function () {
    return meter.low;
  };
}

export function setLow(low) {
  return function (meter) {
    return function () {
      meter.low = low;
    };
  };
}

// ----------------------------------------------------------------------------

export function high(meter) {
  return function () {
    return meter.high;
  };
}

export function setHigh(high) {
  return function (meter) {
    return function () {
      meter.high = high;
    };
  };
}

// ----------------------------------------------------------------------------

export function optimum(meter) {
  return function () {
    return meter.optimum;
  };
}

export function setOptimum(optimum) {
  return function (meter) {
    return function () {
      meter.optimum = optimum;
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(meter) {
  return function () {
    return meter.labels;
  };
}
