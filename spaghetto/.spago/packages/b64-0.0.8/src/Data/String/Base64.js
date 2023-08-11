// Encode a string to its Base64 representation using Node's `Buffer` API
export function encodeNodeImpl (str) {
  const base64EncodedString = Buffer.from(str).toString("base64");

  return base64EncodedString;
};

// Decode a Base64-encoded string using Node's `Buffer` API
export function decodeNodeImpl (Left, Right, str) {
  let result;

  // Check that the input string is a valid Base64-encoded string as Node.js
  // decided that it would be a good idea to NOT throw on invalid input strings
  // but return an empty buffer instead which cannot be distinguished from the
  // empty string case.
  const reEmptyString = "^$";
  const leadingQuanta = "^([A-Za-z0-9+/]{4})*";
  const finalQuantum =
    "([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}(?:=)?|[A-Za-z0-9+/]{2}(?:=){0,2})$";
  const reValidBase64 =
    new RegExp([reEmptyString, "|", leadingQuanta, finalQuantum].join(""));

  try {
    if (!reValidBase64.test(str)) { throw new Error("Invalid input string");}
    result = Right(Buffer.from(str, "base64").toString("utf-8"));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};

export function atobImpl (Left, Right, str) {
  let result;

  try {
    result = Right(atob(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};

export function btoaImpl (Left, Right, str) {
  let result;

  try {
    result = Right(btoa(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};
