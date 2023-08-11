export function colSpan(cell) {
  return function () {
    return cell.colSpan;
  };
}

export function setColSpan(colSpan) {
  return function (cell) {
    return function () {
      cell.colSpan = colSpan;
    };
  };
}

// ----------------------------------------------------------------------------

export function rowSpan(cell) {
  return function () {
    return cell.rowSpan;
  };
}

export function setRowSpan(rowSpan) {
  return function (cell) {
    return function () {
      cell.rowSpan = rowSpan;
    };
  };
}

// ----------------------------------------------------------------------------

export function cellIndex(cell) {
  return function () {
    return cell.cellIndex;
  };
}
