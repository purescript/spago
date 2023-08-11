export function disabled(option) {
  return function () {
    return option.disabled;
  };
}

export function setDisabled(disabled) {
  return function (option) {
    return function () {
      option.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(option) {
  return function () {
    return option.form;
  };
}

// ----------------------------------------------------------------------------

export function label(option) {
  return function () {
    return option.label;
  };
}

export function setLabel(label) {
  return function (option) {
    return function () {
      option.label = label;
    };
  };
}

// ----------------------------------------------------------------------------

export function defaultSelected(option) {
  return function () {
    return option.defaultSelected;
  };
}

export function setDefaultSelected(defaultSelected) {
  return function (option) {
    return function () {
      option.defaultSelected = defaultSelected;
    };
  };
}

// ----------------------------------------------------------------------------

export function selected(option) {
  return function () {
    return option.selected;
  };
}

export function setSelected(selected) {
  return function (option) {
    return function () {
      option.selected = selected;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(option) {
  return function () {
    return option.value;
  };
}

export function setValue(value) {
  return function (option) {
    return function () {
      option.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function text(option) {
  return function () {
    return option.text;
  };
}

export function setText(text) {
  return function (option) {
    return function () {
      option.text = text;
    };
  };
}

// ----------------------------------------------------------------------------

export function index(option) {
  return function () {
    return option.index;
  };
}
