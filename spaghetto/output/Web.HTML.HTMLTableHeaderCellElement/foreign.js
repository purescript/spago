export function scope(cell) {
  return function () {
    return cell.scope;
  };
}

export function setScope(scope) {
  return function (cell) {
    return function () {
      cell.scope = scope;
    };
  };
}

// ----------------------------------------------------------------------------

export function abbr(cell) {
  return function () {
    return cell.abbr;
  };
}

export function setAbbr(abbr) {
  return function (cell) {
    return function () {
      cell.abbr = abbr;
    };
  };
}
