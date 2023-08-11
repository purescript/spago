export function rowIndex(row) {
  return function () {
    return row.rowIndex;
  };
}

// ----------------------------------------------------------------------------

export function sectionRowIndex(row) {
  return function () {
    return row.sectionRowIndex;
  };
}

// ----------------------------------------------------------------------------

export function cells(row) {
  return function () {
    return row.cells;
  };
}

// ----------------------------------------------------------------------------

export function insertCellAt(index) {
  return function (row) {
    return function () {
      return row.insertCell(index);
    };
  };
}

// ----------------------------------------------------------------------------

export function deleteCell(index) {
  return function (row) {
    return function () {
      row.deleteCell(index);
    };
  };
}
