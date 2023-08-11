"use strict";

export function encodeImpl (utfLabel, str) {
  var encoder = new TextEncoder(utfLabel);

  return encoder.encode(str);
};
