export function language(navigator) {
  return function () {
    return navigator.language;
  };
}

export function languages(navigator) {
  return function () {
    return navigator.languages;
  };
}

export function onLine(navigator) {
  return function () {
    return navigator.onLine;
  };
}

export function platform(navigator) {
  return function () {
    return navigator.platform;
  };
}

export function userAgent(navigator) {
  return function () {
    return navigator.userAgent;
  };
}
