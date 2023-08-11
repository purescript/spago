export function _form(output) {
  return function () {
    return output.form;
  };
}

// ----------------------------------------------------------------------------

export function name(output) {
  return function () {
    return output.name;
  };
}

export function setName(name) {
  return function (output) {
    return function () {
      output.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(output) {
  return function () {
    return output.type;
  };
}

// ----------------------------------------------------------------------------

export function defaultValue(output) {
  return function () {
    return output.defaultValue;
  };
}

export function setDefaultValue(defaultValue) {
  return function (output) {
    return function () {
      output.defaultValue = defaultValue;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(output) {
  return function () {
    return output.value;
  };
}

export function setValue(value) {
  return function (output) {
    return function () {
      output.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function willValidate(output) {
  return function () {
    return output.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(output) {
  return function () {
    return output.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(output) {
  return function () {
    return output.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(output) {
  return function () {
    return output.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(output) {
  return function () {
    return output.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (output) {
    return function () {
      output.setCustomValidity(value);
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(output) {
  return function () {
    return output.labels;
  };
}
