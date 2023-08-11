import parse from "spdx-expression-parse";

export const parseSPDXLicenseIdImpl = (onError, onSuccess, identifier) => {
  try {
    parse(identifier);
    return onSuccess(identifier);
  } catch (_) {
    return onError(`Invalid SPDX identifier ${identifier}`);
  }
};
