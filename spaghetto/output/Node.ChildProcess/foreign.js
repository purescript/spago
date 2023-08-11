/* eslint-env node*/

import { spawn, exec, execFile, execSync, execFileSync, fork as cp_fork } from "child_process";

export function unsafeFromNullable(msg) {
  return x => {
    if (x === null) throw new Error(msg);
    return x;
  };
}

export function spawnImpl(command) {
  return args => opts => () => spawn(command, args, opts);
}

export function execImpl(command) {
  return opts => callback => () => exec(
    command,
    opts,
    (err, stdout, stderr) => {
      callback(err)(stdout)(stderr)();
    }
  );
}

export const execFileImpl = function execImpl(command) {
  return args => opts => callback => () => execFile(
    command,
    args,
    opts,
    (err, stdout, stderr) => {
      callback(err)(stdout)(stderr)();
    }
  );
};

export function execSyncImpl(command) {
  return opts => () => execSync(command, opts);
}

export function execFileSyncImpl(command) {
  return args => opts => () => execFileSync(command, args, opts);
}

export function fork(cmd) {
  return args => () => cp_fork(cmd, args);
}

export function mkOnExit(mkChildExit) {
  return function onExit(cp) {
    return cb => () => {
      cp.on("exit", (code, signal) => {
        cb(mkChildExit(code)(signal))();
      });
    };
  };
}

export function mkOnClose(mkChildExit) {
  return function onClose(cp) {
    return cb => () => {
      cp.on("close", (code, signal) => {
        cb(mkChildExit(code)(signal))();
      });
    };
  };
}

export function onDisconnect(cp) {
  return cb => () => {
    cp.on("disconnect", cb);
  };
}

export function mkOnMessage(nothing) {
  return just => (function onMessage(cp) {
    return cb => () => {
      cp.on("message", (mess, sendHandle) => {
        cb(mess, sendHandle ? just(sendHandle) : nothing)();
      });
    };
  });
}

export function onError(cp) {
  return cb => () => {
    cp.on("error", err => {
      cb(err)();
    });
  };
}

const _undefined = undefined;
export { _undefined as undefined };
import process from "process";
export { process };
