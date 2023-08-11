export function autofocus(button) {
  return function () {
    return button.autofocus;
  };
}

export function setAutofocus(autofocus) {
  return function (button) {
    return function () {
      button.autofocus = autofocus;
    };
  };
}

// ----------------------------------------------------------------------------

export function disabled(button) {
  return function () {
    return button.disabled;
  };
}

export function setDisabled(disabled) {
  return function (button) {
    return function () {
      button.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(button) {
  return function () {
    return button.form;
  };
}

// ----------------------------------------------------------------------------

export function formAction(button) {
  return function () {
    return button.formAction;
  };
}

export function setFormAction(formAction) {
  return function (button) {
    return function () {
      button.formAction = formAction;
    };
  };
}

// ----------------------------------------------------------------------------

export function formEnctype(button) {
  return function () {
    return button.formEnctype;
  };
}

export function setFormEnctype(formEnctype) {
  return function (button) {
    return function () {
      button.formEnctype = formEnctype;
    };
  };
}

// ----------------------------------------------------------------------------

export function formMethod(button) {
  return function () {
    return button.formMethod;
  };
}

export function setFormMethod(formMethod) {
  return function (button) {
    return function () {
      button.formMethod = formMethod;
    };
  };
}

// ----------------------------------------------------------------------------

export function formNoValidate(button) {
  return function () {
    return button.formNoValidate;
  };
}

export function setFormNoValidate(formNoValidate) {
  return function (button) {
    return function () {
      button.formNoValidate = formNoValidate;
    };
  };
}

// ----------------------------------------------------------------------------

export function formTarget(button) {
  return function () {
    return button.formTarget;
  };
}

export function setFormTarget(formTarget) {
  return function (button) {
    return function () {
      button.formTarget = formTarget;
    };
  };
}

// ----------------------------------------------------------------------------

export function name(button) {
  return function () {
    return button.name;
  };
}

export function setName(name) {
  return function (button) {
    return function () {
      button.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(button) {
  return function () {
    return button.type;
  };
}

export function setType(type) {
  return function (button) {
    return function () {
      button.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(button) {
  return function () {
    return button.value;
  };
}

export function setValue(value) {
  return function (button) {
    return function () {
      button.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function willValidate(button) {
  return function () {
    return button.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(button) {
  return function () {
    return button.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(button) {
  return function () {
    return button.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(button) {
  return function () {
    return button.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(button) {
  return function () {
    return button.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (button) {
    return function () {
      button.setCustomValidity(value);
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(button) {
  return function () {
    return button.labels;
  };
}
