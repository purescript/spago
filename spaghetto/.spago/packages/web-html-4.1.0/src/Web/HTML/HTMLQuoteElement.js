export function cite(quote) {
  return function () {
    return quote.cite;
  };
}

export function setCite(cite) {
  return function (quote) {
    return function () {
      quote.cite = cite;
    };
  };
}
