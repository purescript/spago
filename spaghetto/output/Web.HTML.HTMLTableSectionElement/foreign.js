export function rows(section) {
  return function () {
    return section.rows;
  };
}

// ----------------------------------------------------------------------------

export function insertRowAt(index) {
  return function (section) {
    return function () {
      return section.insertRow(index);
    };
  };
}

// ----------------------------------------------------------------------------

export function deleteRow(index) {
  return function (section) {
    return function () {
      section.deleteRow(index);
    };
  };
}
