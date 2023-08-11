export function autofocus(keygen) {
  return function () {
    return keygen.autofocus;
  };
}

export function setAutofocus(autofocus) {
  return function (keygen) {
    return function () {
      keygen.autofocus = autofocus;
    };
  };
}

// ----------------------------------------------------------------------------

export function challenge(keygen) {
  return function () {
    return keygen.challenge;
  };
}

export function setChallenge(challenge) {
  return function (keygen) {
    return function () {
      keygen.challenge = challenge;
    };
  };
}

// ----------------------------------------------------------------------------

export function disabled(keygen) {
  return function () {
    return keygen.disabled;
  };
}

export function setDisabled(disabled) {
  return function (keygen) {
    return function () {
      keygen.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(keygen) {
  return function () {
    return keygen.form;
  };
}

// ----------------------------------------------------------------------------

export function keytype(keygen) {
  return function () {
    return keygen.keytype;
  };
}

export function setKeytype(keytype) {
  return function (keygen) {
    return function () {
      keygen.keytype = keytype;
    };
  };
}

// ----------------------------------------------------------------------------

export function name(keygen) {
  return function () {
    return keygen.name;
  };
}

export function setName(name) {
  return function (keygen) {
    return function () {
      keygen.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(keygen) {
  return function () {
    return keygen.type;
  };
}

// ----------------------------------------------------------------------------

export function willValidate(keygen) {
  return function () {
    return keygen.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(keygen) {
  return function () {
    return keygen.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(keygen) {
  return function () {
    return keygen.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(keygen) {
  return function () {
    return keygen.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(keygen) {
  return function () {
    return keygen.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (keygen) {
    return function () {
      keygen.setCustomValidity(value);
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(keygen) {
  return function () {
    return keygen.labels;
  };
}
