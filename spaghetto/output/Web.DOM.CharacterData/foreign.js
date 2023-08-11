export function data_(t) {
  return function () {
    return t.data;
  };
}

export function length(t) {
  return function () {
    return t.length;
  };
}

export function substringData(offset) {
  return function (count) {
    return function (cd) {
      return function () {
        cd.substringData(offset, count);
      };
    };
  };
}

export function appendData(data) {
  return function (cd) {
    return function () {
      cd.appendData(data);
    };
  };
}

export function insertData(offset) {
  return function (data) {
    return function (cd) {
      return function () {
        cd.insertData(offset, data);
      };
    };
  };
}

export function deleteData(offset) {
  return function (count) {
    return function (cd) {
      return function () {
        cd.deleteData(offset, count);
      };
    };
  };
}

export function replaceData(offset) {
  return function (count) {
    return function (data) {
      return function (cd) {
        return function () {
          cd.replaceData(offset, count, data);
        };
      };
    };
  };
}
