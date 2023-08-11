import { extract } from "tar";

export const extractImpl = (onError, onSuccess, cwd, filename) => () => {
  try {
    extract({
      sync: true,
      cwd: cwd,
      file: filename,
    });
    return onSuccess({});
  } catch (err) {
    return onError(err);
  }
};
