import supportsColorJs from "supports-color";

export const supportsColor = () => {
  // https://no-color.org/
  if ("NO_COLOR" in process.env) {
    return false;
  };

  // https://github.com/purescript/spago/issues/579
  if (supportsColorJs.stderr) {
    return true;
  } else {
    return false;
  };
};
