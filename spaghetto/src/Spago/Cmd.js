import { execa, execaCommand } from "execa";

const mkOptions = (opts) => {
  let options = { preferLocal: true, shell: opts.shell };
  if (opts.cwd) {
    options.cwd = opts.cwd;
  }
  if (opts.input) {
    options.input = opts.input;
  }
  return options;
};

export function spawnImpl(cmd, args, opts) {
  return execa(cmd, args, mkOptions(opts));
}

export function spawnCommandImpl(cmd, opts) {
  return execaCommand(cmd, mkOptions(opts));
};

export function pipeStdinImpl(cp) {
  cp.stdin.pipe(process.stdin);
}

export function pipeStdoutImpl(cp) {
  cp.stdout.pipe(process.stdout);
}
export function pipeStderrImpl(cp) {
  cp.stderr.pipe(process.stderr);
}

export function joinImpl(cp, onError, onSuccess) {
  return cp.then((data) => onSuccess(data))
    .catch((err) => {
      // WTFFFF, this is because on ENOENT we don't get an exitCode
      if (err.exitCode === undefined) {
        err.exitCode = -1;
      }
      return onError(err);
    });
};

export function killImpl(cp) {
  cp.kill('SIGTERM', {
    forceKillAfterTimeout: 2000
  });
}
