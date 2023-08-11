export function unsafeProcessHasProp(prop) {
  return global.process[prop] !== null && global.process[prop] !== undefined;
}

export function unsafeReadProcessProp(prop) {
  return global.process[prop];
}

export function unsafeWriteProcessProp(prop, value) {
  global.process[prop] = value;
}

export function processCallFn(originalProcessReallyExit, exitCode) {
  return originalProcessReallyExit.call(global.process, exitCode);
}

export function processKill(pid, sig) {
  global.process.kill(pid, sig);
}

export function processListenersLength(sig) {
  return global.process.listeners(sig).length;
}

export function processOn(sig, listener) {
  return global.process.on(sig, listener);
}

export function processOff(sig, listener) {
  return global.process.off(sig, listener);
}

export function customProcessEmit(cb) {
  return function (ev, arg) {
    const thisArg = this;
    const argumentsArg = arguments;
    return cb((originalProcessEmit) => originalProcessEmit.apply(thisArg, argumentsArg), ev, arg);
  };
}

export function processExitCode() {
  return global.process.exitCode;
}
