export function _caption(table) {
  return function () {
    return table.caption;
  };
}

export function _setCaption(caption) {
  return function (table) {
    return function () {
      table.caption = caption;
    };
  };
}

// ----------------------------------------------------------------------------

export function createCaption(table) {
  return function () {
    return table.createCaption();
  };
}

// ----------------------------------------------------------------------------

export function deleteCaption(table) {
  return function () {
    table.deleteCaption();
  };
}

// ----------------------------------------------------------------------------

export function _tHead(table) {
  return function () {
    return table.tHead;
  };
}

export function _setTHead(tHead) {
  return function (table) {
    return function () {
      table.tHead = tHead;
    };
  };
}

// ----------------------------------------------------------------------------

export function createTHead(table) {
  return function () {
    return table.createTHead();
  };
}

// ----------------------------------------------------------------------------

export function deleteTHead(table) {
  return function () {
    table.deleteTHead();
  };
}

// ----------------------------------------------------------------------------

export function _tFoot(table) {
  return function () {
    return table.tFoot;
  };
}

export function _setTFoot(tFoot) {
  return function (table) {
    return function () {
      table.tFoot = tFoot;
    };
  };
}

// ----------------------------------------------------------------------------

export function createTFoot(table) {
  return function () {
    return table.createTFoot();
  };
}

// ----------------------------------------------------------------------------

export function deleteTFoot(table) {
  return function () {
    table.deleteTFoot();
  };
}

// ----------------------------------------------------------------------------

export function tBodies(table) {
  return function () {
    return table.tBodies;
  };
}

// ----------------------------------------------------------------------------

export function createTBody(table) {
  return function () {
    return table.createTBody();
  };
}

// ----------------------------------------------------------------------------

export function rows(table) {
  return function () {
    return table.rows;
  };
}

// ----------------------------------------------------------------------------

export function insertRowAt(index) {
  return function (table) {
    return function () {
      return table.insertRow(index);
    };
  };
}

// ----------------------------------------------------------------------------

export function deleteRow(index) {
  return function (table) {
    return function () {
      table.deleteRow(index);
    };
  };
}

// ----------------------------------------------------------------------------

export function border(table) {
  return function () {
    return table.border;
  };
}

export function setBorder(border) {
  return function (table) {
    return function () {
      table.border = border;
    };
  };
}
