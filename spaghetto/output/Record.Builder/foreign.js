export function copyRecord(rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
}

export function unsafeInsert(l) {
  return function(a) {
    return function(rec) {
      rec[l] = a;
      return rec;
    };
  };
}

export function unsafeModify(l) {
  return function (f) {
    return function(rec) {
      rec[l] = f(rec[l]);
      return rec;
    };
  };
}

export function unsafeDelete(l) {
  return function(rec) {
    delete rec[l];
    return rec;
  };
}

export function unsafeRename(l1) {
  return function (l2) {
    return function (rec) {
      rec[l2] = rec[l1];
      delete rec[l1];
      return rec;
    };
  };
}
