export function disabled(optgroup) {
  return function () {
    return optgroup.disabled;
  };
}

export function setDisabled(disabled) {
  return function (optgroup) {
    return function () {
      optgroup.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function label(optgroup) {
  return function () {
    return optgroup.label;
  };
}

export function setLabel(label) {
  return function (optgroup) {
    return function () {
      optgroup.label = label;
    };
  };
}
