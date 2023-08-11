export function autocomplete(textarea) {
  return function () {
    return textarea.autocomplete;
  };
}

export function setAutocomplete(autocomplete) {
  return function (textarea) {
    return function () {
      textarea.autocomplete = autocomplete;
    };
  };
}

// ----------------------------------------------------------------------------

export function autofocus(textarea) {
  return function () {
    return textarea.autofocus;
  };
}

export function setAutofocus(autofocus) {
  return function (textarea) {
    return function () {
      textarea.autofocus = autofocus;
    };
  };
}

// ----------------------------------------------------------------------------

export function cols(textarea) {
  return function () {
    return textarea.cols;
  };
}

export function setCols(cols) {
  return function (textarea) {
    return function () {
      textarea.cols = cols;
    };
  };
}

// ----------------------------------------------------------------------------

export function dirName(textarea) {
  return function () {
    return textarea.dirName;
  };
}

export function setDirName(dirName) {
  return function (textarea) {
    return function () {
      textarea.dirName = dirName;
    };
  };
}

// ----------------------------------------------------------------------------

export function disabled(textarea) {
  return function () {
    return textarea.disabled;
  };
}

export function setDisabled(disabled) {
  return function (textarea) {
    return function () {
      textarea.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(textarea) {
  return function () {
    return textarea.form;
  };
}

// ----------------------------------------------------------------------------

export function maxLength(textarea) {
  return function () {
    return textarea.maxLength;
  };
}

export function setMaxLength(maxLength) {
  return function (textarea) {
    return function () {
      textarea.maxLength = maxLength;
    };
  };
}

// ----------------------------------------------------------------------------

export function minLength(textarea) {
  return function () {
    return textarea.minLength;
  };
}

export function setMinLength(minLength) {
  return function (textarea) {
    return function () {
      textarea.minLength = minLength;
    };
  };
}

// ----------------------------------------------------------------------------

export function name(textarea) {
  return function () {
    return textarea.name;
  };
}

export function setName(name) {
  return function (textarea) {
    return function () {
      textarea.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function placeholder(textarea) {
  return function () {
    return textarea.placeholder;
  };
}

export function setPlaceholder(placeholder) {
  return function (textarea) {
    return function () {
      textarea.placeholder = placeholder;
    };
  };
}

// ----------------------------------------------------------------------------

export function readOnly(textarea) {
  return function () {
    return textarea.readOnly;
  };
}

export function setReadOnly(readOnly) {
  return function (textarea) {
    return function () {
      textarea.readOnly = readOnly;
    };
  };
}

// ----------------------------------------------------------------------------

export function required(textarea) {
  return function () {
    return textarea.required;
  };
}

export function setRequired(required) {
  return function (textarea) {
    return function () {
      textarea.required = required;
    };
  };
}

// ----------------------------------------------------------------------------

export function rows(textarea) {
  return function () {
    return textarea.rows;
  };
}

export function setRows(rows) {
  return function (textarea) {
    return function () {
      textarea.rows = rows;
    };
  };
}

// ----------------------------------------------------------------------------

export function wrap(textarea) {
  return function () {
    return textarea.wrap;
  };
}

export function setWrap(wrap) {
  return function (textarea) {
    return function () {
      textarea.wrap = wrap;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(textarea) {
  return function () {
    return textarea.type;
  };
}

// ----------------------------------------------------------------------------

export function defaultValue(textarea) {
  return function () {
    return textarea.defaultValue;
  };
}

export function setDefaultValue(defaultValue) {
  return function (textarea) {
    return function () {
      textarea.defaultValue = defaultValue;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(textarea) {
  return function () {
    return textarea.value;
  };
}

export function setValue(value) {
  return function (textarea) {
    return function () {
      textarea.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function textLength(textarea) {
  return function () {
    return textarea.textLength;
  };
}

// ----------------------------------------------------------------------------

export function willValidate(textarea) {
  return function () {
    return textarea.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(textarea) {
  return function () {
    return textarea.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(textarea) {
  return function () {
    return textarea.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(textarea) {
  return function () {
    return textarea.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(textarea) {
  return function () {
    return textarea.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (textarea) {
    return function () {
      textarea.setCustomValidity(value);
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(textarea) {
  return function () {
    return textarea.labels;
  };
}

// ----------------------------------------------------------------------------

export function select(textarea) {
  return function () {
    textarea.select();
  };
}

// ----------------------------------------------------------------------------

export function selectionStart(textarea) {
  return function () {
    return textarea.selectionStart;
  };
}

export function setSelectionStart(selectionStart) {
  return function (textarea) {
    return function () {
      textarea.selectionStart = selectionStart;
    };
  };
}

// ----------------------------------------------------------------------------

export function selectionEnd(textarea) {
  return function () {
    return textarea.selectionEnd;
  };
}

export function setSelectionEnd(selectionEnd) {
  return function (textarea) {
    return function () {
      textarea.selectionEnd = selectionEnd;
    };
  };
}

// ----------------------------------------------------------------------------

export function selectionDirection(textarea) {
  return function () {
    return textarea.selectionDirection;
  };
}

export function setSelectionDirection(selectionDirection) {
  return function (textarea) {
    return function () {
      textarea.selectionDirection = selectionDirection;
    };
  };
}

// ----------------------------------------------------------------------------

export function setRangeText(replacement) {
  return function (textarea) {
    return function () {
      textarea.setRangeText(replacement);
    };
  };
}

export function _setRangeText(replacement, start, end, selectionMode, textarea) {
  textarea.setRangeText(replacement, start, end, selectionMode);
}

// ----------------------------------------------------------------------------

export function setSelectionRange(start) {
  return function (end) {
    return function (direction) {
      return function (textarea) {
        return function () {
          textarea.setSelectionRange(start, end, direction);
        };
      };
    };
  };
}
