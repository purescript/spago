import fs from 'fs-extra';
const { moveSync } = fs;

export const moveSyncImpl = (source) => (destination) => () => moveSync(source, destination);
