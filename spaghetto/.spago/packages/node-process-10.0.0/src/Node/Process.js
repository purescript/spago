import process from "process";
export { process };

export function onBeforeExit(callback) {
  return () => {
    process.on("beforeExit", callback);
  };
}

export function onExit(callback) {
  return () => {
    process.on("exit", code => {
      callback(code)();
    });
  };
}

export function onUncaughtException(callback) {
  return () => {
    process.on("uncaughtException", error => {
      callback(error)();
    });
  };
}

export function onUnhandledRejection(callback) {
  return () => {
    process.on("unhandledRejection", (error, promise) => {
      callback(error)(promise)();
    });
  };
}

export function onSignalImpl(signal) {
  return callback => () => {
    process.on(signal, callback);
  };
}

export function chdir(dir) {
  return () => {
    process.chdir(dir);
  };
}

export function setEnv(var_) {
  return val => () => {
    process.env[var_] = val;
  };
}

export function unsetEnv(var_) {
  return () => {
    delete process.env[var_];
  };
}

export function exit(code) {
  return () => {
    process.exit(code);
  };
}

export function copyArray(xs) {
  return () => xs.slice();
}

export function copyObject(o) {
  return () => Object.assign({}, o);
}
