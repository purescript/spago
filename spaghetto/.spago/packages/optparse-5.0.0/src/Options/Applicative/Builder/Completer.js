"use strict";
import execSync from 'child_process';

export function execSyncCommand (command) {
  return function() {
    return execSync(command);
  };
};
