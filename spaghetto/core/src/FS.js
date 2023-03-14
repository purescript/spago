import fs from 'fs-extra';
const { moveSync, ensureFileSync } = fs;

export const moveSyncImpl = (source) => (destination) => () => moveSync(source, destination);

export const ensureFileSyncImpl = (file) => () => ensureFileSync(file);
