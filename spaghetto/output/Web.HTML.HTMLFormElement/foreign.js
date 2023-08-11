export function acceptCharset(form) {
  return function () {
    return form.acceptCharset;
  };
}

export function setAcceptCharset(acceptCharset) {
  return function (form) {
    return function () {
      form.acceptCharset = acceptCharset;
    };
  };
}

// ----------------------------------------------------------------------------

export function action(form) {
  return function () {
    return form.action;
  };
}

export function setAction(action) {
  return function (form) {
    return function () {
      form.action = action;
    };
  };
}

// ----------------------------------------------------------------------------

export function autocomplete(form) {
  return function () {
    return form.autocomplete;
  };
}

export function setAutocomplete(autocomplete) {
  return function (form) {
    return function () {
      form.autocomplete = autocomplete;
    };
  };
}

// ----------------------------------------------------------------------------

export function enctype(form) {
  return function () {
    return form.enctype;
  };
}

export function setEnctype(enctype) {
  return function (form) {
    return function () {
      form.enctype = enctype;
    };
  };
}

// ----------------------------------------------------------------------------

export function encoding(form) {
  return function () {
    return form.encoding;
  };
}

export function setEncoding(encoding) {
  return function (form) {
    return function () {
      form.encoding = encoding;
    };
  };
}

// ----------------------------------------------------------------------------

export function method(form) {
  return function () {
    return form.method;
  };
}

export function setMethod(method) {
  return function (form) {
    return function () {
      form.method = method;
    };
  };
}

// ----------------------------------------------------------------------------

export function name(form) {
  return function () {
    return form.name;
  };
}

export function setName(name) {
  return function (form) {
    return function () {
      form.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function noValidate(form) {
  return function () {
    return form.noValidate;
  };
}

export function setNoValidate(noValidate) {
  return function (form) {
    return function () {
      form.noValidate = noValidate;
    };
  };
}

// ----------------------------------------------------------------------------

export function target(form) {
  return function () {
    return form.target;
  };
}

export function setTarget(target) {
  return function (form) {
    return function () {
      form.target = target;
    };
  };
}

// ----------------------------------------------------------------------------

export function length(form) {
  return function () {
    return form.length;
  };
}

// ----------------------------------------------------------------------------

export function submit(form) {
  return function () {
    form.submit();
  };
}

// ----------------------------------------------------------------------------

export function reset(form) {
  return function () {
    form.reset();
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(form) {
  return function () {
    return form.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(form) {
  return function () {
    return form.reportValidity();
  };
}
