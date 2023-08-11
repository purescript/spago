export function _form(label) {
  return function () {
    return label.form;
  };
}

// ----------------------------------------------------------------------------

export function htmlFor(label) {
  return function () {
    return label.htmlFor;
  };
}

export function setHtmlFor(htmlFor) {
  return function (label) {
    return function () {
      label.htmlFor = htmlFor;
    };
  };
}

// ----------------------------------------------------------------------------

export function _control(label) {
  return function () {
    return label.control;
  };
}
