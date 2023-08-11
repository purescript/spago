export function text(title) {
  return function () {
    return title.text;
  };
}

export function setText(text) {
  return function (title) {
    return function () {
      title.text = text;
    };
  };
}
