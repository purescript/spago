export function cite(mod) {
  return function () {
    return mod.cite;
  };
}

export function setCite(cite) {
  return function (mod) {
    return function () {
      mod.cite = cite;
    };
  };
}

// ----------------------------------------------------------------------------

export function dateTime(mod) {
  return function () {
    return mod.dateTime;
  };
}

export function setDateTime(dateTime) {
  return function (mod) {
    return function () {
      mod.dateTime = dateTime;
    };
  };
}
