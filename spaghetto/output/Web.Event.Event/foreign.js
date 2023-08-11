export function bubbles(e) {
  return e.bubbles;
}

export function cancelable(e) {
  return e.cancelable;
}

export function _currentTarget(e) {
  return e.currentTarget;
}

export function defaultPrevented(e) {
  return function() {
    return e.defaultPrevented;
  };
}

export function eventPhaseIndex(e) {
  return e.eventPhase;
}

export function _target(e) {
  return e.target;
}

export function timeStamp(e) {
  return e.timeStamp;
}

export function type_(e) {
  return e.type;
}

export function preventDefault(e) {
  return function () {
    return e.preventDefault();
  };
}

export function stopImmediatePropagation(e) {
  return function () {
    return e.stopImmediatePropagation();
  };
}

export function stopPropagation(e) {
  return function () {
    return e.stopPropagation();
  };
}
