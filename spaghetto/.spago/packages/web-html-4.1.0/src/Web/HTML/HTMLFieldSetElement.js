export function disabled(fieldset) {
  return function () {
    return fieldset.disabled;
  };
}

export function setDisabled(disabled) {
  return function (fieldset) {
    return function () {
      fieldset.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(fieldset) {
  return function () {
    return fieldset.form;
  };
}

// ----------------------------------------------------------------------------

export function name(fieldset) {
  return function () {
    return fieldset.name;
  };
}

export function setName(name) {
  return function (fieldset) {
    return function () {
      fieldset.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(fieldset) {
  return function () {
    return fieldset.type;
  };
}

export function setType(type) {
  return function (fieldset) {
    return function () {
      fieldset.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function willValidate(fieldset) {
  return function () {
    return fieldset.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(fieldset) {
  return function () {
    return fieldset.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(fieldset) {
  return function () {
    return fieldset.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(fieldset) {
  return function () {
    return fieldset.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(fieldset) {
  return function () {
    return fieldset.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (fieldset) {
    return function () {
      fieldset.setCustomValidity(value);
    };
  };
}
