export function accept(input) {
  return function () {
    return input.accept;
  };
}

export function setAccept(accept) {
  return function (input) {
    return function () {
      input.accept = accept;
    };
  };
}

// ----------------------------------------------------------------------------

export function alt(input) {
  return function () {
    return input.alt;
  };
}

export function setAlt(alt) {
  return function (input) {
    return function () {
      input.alt = alt;
    };
  };
}

// ----------------------------------------------------------------------------

export function autocomplete(input) {
  return function () {
    return input.autocomplete;
  };
}

export function setAutocomplete(autocomplete) {
  return function (input) {
    return function () {
      input.autocomplete = autocomplete;
    };
  };
}

// ----------------------------------------------------------------------------

export function autofocus(input) {
  return function () {
    return input.autofocus;
  };
}

export function setAutofocus(autofocus) {
  return function (input) {
    return function () {
      input.autofocus = autofocus;
    };
  };
}

// ----------------------------------------------------------------------------

export function defaultChecked(input) {
  return function () {
    return input.defaultChecked;
  };
}

export function setDefaultChecked(defaultChecked) {
  return function (input) {
    return function () {
      input.defaultChecked = defaultChecked;
    };
  };
}

// ----------------------------------------------------------------------------

export function checked(input) {
  return function () {
    return input.checked;
  };
}

export function setChecked(checked) {
  return function (input) {
    return function () {
      input.checked = checked;
    };
  };
}

// ----------------------------------------------------------------------------

export function dirName(input) {
  return function () {
    return input.dirName;
  };
}

export function setDirName(dirName) {
  return function (input) {
    return function () {
      input.dirName = dirName;
    };
  };
}

// ----------------------------------------------------------------------------

export function disabled(input) {
  return function () {
    return input.disabled;
  };
}

export function setDisabled(disabled) {
  return function (input) {
    return function () {
      input.disabled = disabled;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(input) {
  return function () {
    return input.form;
  };
}

// ----------------------------------------------------------------------------

export function _files(input) {
  return function () {
    return input.files;
  };
}

// ----------------------------------------------------------------------------

export function formAction(input) {
  return function () {
    return input.formAction;
  };
}

export function setFormAction(formAction) {
  return function (input) {
    return function () {
      input.formAction = formAction;
    };
  };
}

// ----------------------------------------------------------------------------

export function formEnctype(input) {
  return function () {
    return input.formEnctype;
  };
}

export function setFormEnctype(formEnctype) {
  return function (input) {
    return function () {
      input.formEnctype = formEnctype;
    };
  };
}

// ----------------------------------------------------------------------------

export function formMethod(input) {
  return function () {
    return input.formMethod;
  };
}

export function setFormMethod(formMethod) {
  return function (input) {
    return function () {
      input.formMethod = formMethod;
    };
  };
}

// ----------------------------------------------------------------------------

export function formNoValidate(input) {
  return function () {
    return input.formNoValidate;
  };
}

export function setFormNoValidate(formNoValidate) {
  return function (input) {
    return function () {
      input.formNoValidate = formNoValidate;
    };
  };
}

// ----------------------------------------------------------------------------

export function formTarget(input) {
  return function () {
    return input.formTarget;
  };
}

export function setFormTarget(formTarget) {
  return function (input) {
    return function () {
      input.formTarget = formTarget;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(input) {
  return function () {
    return input.height;
  };
}

export function setHeight(height) {
  return function (input) {
    return function () {
      input.height = height;
    };
  };
}

// ----------------------------------------------------------------------------

export function indeterminate(input) {
  return function () {
    return input.indeterminate;
  };
}

export function setIndeterminate(indeterminate) {
  return function (input) {
    return function () {
      input.indeterminate = indeterminate;
    };
  };
}

// ----------------------------------------------------------------------------

export function _list(input) {
  return function () {
    return input.list;
  };
}

// ----------------------------------------------------------------------------

export function max(input) {
  return function () {
    return input.max;
  };
}

export function setMax(max) {
  return function (input) {
    return function () {
      input.max = max;
    };
  };
}

// ----------------------------------------------------------------------------

export function maxLength(input) {
  return function () {
    return input.maxLength;
  };
}

export function setMaxLength(maxLength) {
  return function (input) {
    return function () {
      input.maxLength = maxLength;
    };
  };
}

// ----------------------------------------------------------------------------

export function min(input) {
  return function () {
    return input.min;
  };
}

export function setMin(min) {
  return function (input) {
    return function () {
      input.min = min;
    };
  };
}

// ----------------------------------------------------------------------------

export function minLength(input) {
  return function () {
    return input.minLength;
  };
}

export function setMinLength(minLength) {
  return function (input) {
    return function () {
      input.minLength = minLength;
    };
  };
}

// ----------------------------------------------------------------------------

export function multiple(input) {
  return function () {
    return input.multiple;
  };
}

export function setMultiple(multiple) {
  return function (input) {
    return function () {
      input.multiple = multiple;
    };
  };
}

// ----------------------------------------------------------------------------

export function name(input) {
  return function () {
    return input.name;
  };
}

export function setName(name) {
  return function (input) {
    return function () {
      input.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function pattern(input) {
  return function () {
    return input.pattern;
  };
}

export function setPattern(pattern) {
  return function (input) {
    return function () {
      input.pattern = pattern;
    };
  };
}

// ----------------------------------------------------------------------------

export function placeholder(input) {
  return function () {
    return input.placeholder;
  };
}

export function setPlaceholder(placeholder) {
  return function (input) {
    return function () {
      input.placeholder = placeholder;
    };
  };
}

// ----------------------------------------------------------------------------

export function readOnly(input) {
  return function () {
    return input.readOnly;
  };
}

export function setReadOnly(readOnly) {
  return function (input) {
    return function () {
      input.readOnly = readOnly;
    };
  };
}

// ----------------------------------------------------------------------------

export function required(input) {
  return function () {
    return input.required;
  };
}

export function setRequired(required) {
  return function (input) {
    return function () {
      input.required = required;
    };
  };
}

// ----------------------------------------------------------------------------

export function size(input) {
  return function () {
    return input.size;
  };
}

export function setSize(size) {
  return function (input) {
    return function () {
      input.size = size;
    };
  };
}

// ----------------------------------------------------------------------------

export function src(input) {
  return function () {
    return input.src;
  };
}

export function setSrc(src) {
  return function (input) {
    return function () {
      input.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function step(input) {
  return function () {
    return input.step;
  };
}

export function setStep(step) {
  return function (input) {
    return function () {
      input.step = step;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(input) {
  return function () {
    return input.type;
  };
}

export function setType(type) {
  return function (input) {
    return function () {
      input.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function defaultValue(input) {
  return function () {
    return input.defaultValue;
  };
}

export function setDefaultValue(defaultValue) {
  return function (input) {
    return function () {
      input.defaultValue = defaultValue;
    };
  };
}

// ----------------------------------------------------------------------------

export function value(input) {
  return function () {
    return input.value;
  };
}

export function setValue(value) {
  return function (input) {
    return function () {
      input.value = value;
    };
  };
}

// ----------------------------------------------------------------------------

export function valueAsDate(input) {
  return function () {
    return input.valueAsDate;
  };
}

export function setValueAsDate(valueAsDate) {
  return function (input) {
    return function () {
      input.valueAsDate = valueAsDate;
    };
  };
}

// ----------------------------------------------------------------------------

export function valueAsNumber(input) {
  return function () {
    return input.valueAsNumber;
  };
}

export function setValueAsNumber(valueAsNumber) {
  return function (input) {
    return function () {
      input.valueAsNumber = valueAsNumber;
    };
  };
}

// ----------------------------------------------------------------------------

export function width(input) {
  return function () {
    return input.width;
  };
}

export function setWidth(width) {
  return function (input) {
    return function () {
      input.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function stepUpBy(n) {
  return function (input) {
    return function () {
      input.stepUp(n);
    };
  };
}

// ----------------------------------------------------------------------------

export function stepDownBy(n) {
  return function (input) {
    return function () {
      input.stepDown(n);
    };
  };
}

// ----------------------------------------------------------------------------

export function willValidate(input) {
  return function () {
    return input.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(input) {
  return function () {
    return input.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(input) {
  return function () {
    return input.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(input) {
  return function () {
    return input.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(input) {
  return function () {
    return input.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (input) {
    return function () {
      input.setCustomValidity(value);
    };
  };
}

// ----------------------------------------------------------------------------

export function labels(input) {
  return function () {
    return input.labels;
  };
}

// ----------------------------------------------------------------------------

export function select(input) {
  return function () {
    input.select();
  };
}

// ----------------------------------------------------------------------------

export function selectionStart(input) {
  return function () {
    return input.selectionStart;
  };
}

export function setSelectionStart(selectionStart) {
  return function (input) {
    return function () {
      input.selectionStart = selectionStart;
    };
  };
}

// ----------------------------------------------------------------------------

export function selectionEnd(input) {
  return function () {
    return input.selectionEnd;
  };
}

export function setSelectionEnd(selectionEnd) {
  return function (input) {
    return function () {
      input.selectionEnd = selectionEnd;
    };
  };
}

// ----------------------------------------------------------------------------

export function selectionDirection(input) {
  return function () {
    return input.selectionDirection;
  };
}

export function setSelectionDirection(selectionDirection) {
  return function (input) {
    return function () {
      input.selectionDirection = selectionDirection;
    };
  };
}

// ----------------------------------------------------------------------------

export function setRangeText(replacement) {
  return function (input) {
    return function () {
      input.setRangeText(replacement);
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
      return function (input) {
        return function () {
          input.setSelectionRange(start, end, direction, input);
        };
      };
    };
  };
}
