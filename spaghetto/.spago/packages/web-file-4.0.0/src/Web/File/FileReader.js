export function fileReader() {
  return new FileReader();
}

export function error(fr) {
  return function () {
    return fr.error;
  };
}

export function readyStateImpl(fr) {
  return function () {
    return fr.readyState;
  };
}

export function result(fr) {
  return function () {
    return fr.result;
  };
}

export function abort(fr) {
  return function () {
    fr.abort();
  };
}

export function readAsText(blob) {
  return function (fr) {
    return function () {
      fr.readAsText(blob);
    };
  };
}

export function readAsArrayBuffer(blob) {
  return function (fr) {
    return function () {
      fr.readAsArrayBuffer(blob);
    };
  };
}

export function readAsDataURL(blob) {
  return function (fr) {
    return function () {
      fr.readAsDataURL(blob);
    };
  };
}
