export function data_(object) {
  return function () {
    return object.data;
  };
}

export function setData(data) {
  return function (object) {
    return function () {
      object.data = data;
    };
  };
}

// ----------------------------------------------------------------------------

export function type_(object) {
  return function () {
    return object.type;
  };
}

export function setType(type) {
  return function (object) {
    return function () {
      object.type = type;
    };
  };
}

// ----------------------------------------------------------------------------

export function typeMustMatch(object) {
  return function () {
    return object.typeMustMatch;
  };
}

// ----------------------------------------------------------------------------

export function name(object) {
  return function () {
    return object.name;
  };
}

export function setName(name) {
  return function (object) {
    return function () {
      object.name = name;
    };
  };
}

// ----------------------------------------------------------------------------

export function useMap(object) {
  return function () {
    return object.useMap;
  };
}

export function setUseMap(useMap) {
  return function (object) {
    return function () {
      object.useMap = useMap;
    };
  };
}

// ----------------------------------------------------------------------------

export function _form(object) {
  return function () {
    return object.form;
  };
}

// ----------------------------------------------------------------------------

export function width(object) {
  return function () {
    return object.width;
  };
}

export function setWidth(width) {
  return function (object) {
    return function () {
      object.width = width;
    };
  };
}

// ----------------------------------------------------------------------------

export function height(object) {
  return function () {
    return object.height;
  };
}

export function setHeight(height) {
  return function (object) {
    return function () {
      object.height = height;
    };
  };
}

// ----------------------------------------------------------------------------

export function _contentDocument(object) {
  return function () {
    return object.contentDocument;
  };
}

// ----------------------------------------------------------------------------

export function willValidate(object) {
  return function () {
    return object.willValidate;
  };
}

// ----------------------------------------------------------------------------

export function validity(object) {
  return function () {
    return object.validity;
  };
}

// ----------------------------------------------------------------------------

export function validationMessage(object) {
  return function () {
    return object.validationMessage;
  };
}

// ----------------------------------------------------------------------------

export function checkValidity(object) {
  return function () {
    return object.checkValidity();
  };
}

// ----------------------------------------------------------------------------

export function reportValidity(object) {
  return function () {
    return object.reportValidity();
  };
}

// ----------------------------------------------------------------------------

export function setCustomValidity(value) {
  return function (object) {
    return function () {
      object.setCustomValidity(value);
    };
  };
}
