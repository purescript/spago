import { extract } from "tar";

export const extractImpl = (cwd, filename) => () => {
  extract({
    sync: true,
    cwd: cwd,
    file: filename,
  });
};
