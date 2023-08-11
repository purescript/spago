export function add(list) {
  return function(token) {
    return function() {
      return list.add(token);
    };
  };
}

export function remove(list) {
  return function(token) {
    return function() {
      return list.remove(token);
    };
  };
}

export function contains(list) {
  return function(token) {
    return function() {
      return list.contains(token);
    };
  };
}

export function toggle(list) {
  return function(token) {
    return function() {
      return list.toggle(token);
    };
  };
}

export function toggleForce(list) {
  return function(token) {
    return function(force) {
      return function() {
        return list.toggle(token, force);
      };
    };
  };
}

export function _item(list) {
  return function(index) {
    return function() {
      return list.item(index);
    };
  };
}
