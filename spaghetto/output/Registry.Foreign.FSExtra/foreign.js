import { ensureDir, remove, copy } from "fs-extra";

export const ensureDirectoryImpl = (path) => () => ensureDir(path);

export const removeImpl = (path) => () => remove(path);

export const copyImpl = (source) => (destination) => (options) => () =>
  copy(source, destination, options);
