"use strict";

export function decodeImpl (Left, Right, utfLabel, buffer) {
  var result;
  var decoder = new TextDecoder(utfLabel);

  try {
    result = Right(decoder.decode(buffer));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};
