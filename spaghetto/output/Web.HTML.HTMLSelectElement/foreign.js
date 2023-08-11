export function autofocus(select) {
  return function () {
    return select.autofocus;
  };
}

export function setAutofocus(autofocus) {
  return function (select) {
    return function () {
      select.autofocus = autofocus;
    };
  };
}

// ----------------------------------------------------------------------------

export function disabled(select) {
  return function () {
    return select.disabled;
  };
}

export function setDisabled(disabled) {
  return function (select) {
    return function () {
      select.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function multiple(select) {
  return function () {
    return select.multiple;
  };
}

export function setMultiple(multiple) {
  return function (select) {
    return function () {
      select.multiple = multiple;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(select) {
  return function () {
    return select.form;
  };
}

// ----------------------------------------------------------------------------

export function name(select) {
  return function () {
    return select.name;
  };
}

export function setName(name) {
  return function (select) {
    return function () {
      select.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function required(select) {
  return function () {
    return select.required;
  };
}

export function setRequired(required) {
  return function (select) {
    return function () {
      select.required = required;
    };
  };
}

// ----------------------------------------------------------------------------

export function size(select) {
  return function () {
    return select.size;
  };
}

export function setSize(size) {
  return function (select) {
    return function () {
      select.size = size;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(select) {
  return function () {
    return select.type;
  };
}

// ----------------------------------------------------------------------------

export function length(select) {
  return function () {
    return select.length;
  };
}

export function setLength(length) {
  return function (select) {
    return function () {
      select.length = length;
    };
  };
}

// ----------------------------------------------------------------------------

export function selectedOptions(select) {
  return function () {
    return select.selectedOptions;
  };
}

// ----------------------------------------------------------------------------

export function selectedIndex(select) {
  return function () {
    return select.selectedIndex;
  };
}

export function setSelectedIndex(selectedIndex) {
  return function (select) {
    return function () {
      select.selectedIndex = selectedIndex;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(select) {
  return function () {
    return select.value;
  };
}

export function setValue(value) {
  return function (select) {
    return function () {
      select.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function willValidate(select) {
  return function () {
    return select.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(select) {
  return function () {
    return select.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(select) {
  return function () {
    return select.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(select) {
  return function () {
    return select.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(select) {
  return function () {
    return select.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (select) {
    return function () {
      select.setCustomValidity(value);
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(select) {
  return function () {
    return select.labels;
  };
}
