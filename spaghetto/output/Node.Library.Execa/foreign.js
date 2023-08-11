// Unless otherwise stated, FFI here is
// for child process-related things
import { Readable } from "node:stream";

export { 
  spawn as spawnImpl, 
  spawnSync as spawnSyncImpl
} from "node:child_process";

// For execa-specifically.
export function monkeyPatchKill(cp, killFn) {
  cp.kill = killFn.bind(null, cp.kill.bind(cp))
}

// For execa-specifically.
export function setTimeoutImpl(timeout, cb) {
  const t = setTimeout(cb, timeout);
  // Guarded because there's no `.unref()` when `execa` is used in the renderer
	// process in Electron. This cannot be tested since we don't run tests in
	// Electron.
  return t.unref ? t : { unref: () => {} };
}

export function channelImpl(cp) {
  return cp.channel;
}

export function connectedImpl(cp) {
  return cp.connected;
}

export function disconnectImpl(cp) {
  return cp.disconnect();
}

export function exitCodeImpl(cp) {
  return cp.exitCode;
}

export function killImpl(cp, signal, options) {
  return cp.kill(signal, options);
}

export function pidExistsImpl(cp) {
  return cp.kill(0);
}

export function killedImpl(cp) {
  return cp.killed;
}

export function pidImpl(cp) {
  return cp.pid;
}

export function refImpl(cp) {
  return cp.ref();
}

export function unrefImpl(cp) {
  return cp.unref();
}

export function sendImpl(cp, msg, handle, options, cb) {
  return cp.send(msg, handle, options, cb);
}

export function signalCodeImpl(cp) {
  return cp.signalCode;
}

export function spawnArgs(cp) {
  return cp.spawnArgs;
}

export function spawnFile(cp) {
  return cp.spawnFile;
}

export function stderr(cp) {
  return cp.stderr;
}

export function stdin(cp) {
  return cp.stdin;
}

export function stdioImpl(cp) {
  return cp.stdio;
}

export function stdout(cp) {
  return cp.stdout;
}

export function onCloseImpl(cp, cb) {
  return cp.on("close", cb);
}

export function onDisconnectImpl(cp, cb) {
  return cp.on("disconnect", cb);
}

export function onErrorImpl(cp, cb) {
  return cp.on("error", cb);
}

export function onExitImpl(cp, cb) {
  return cp.on("exit", cb);
}

export function onMessageImpl(cp, cb) {
  return cp.on("message", cb);
}

export function onSpawnImpl(cp, cb) {
  return cp.on("spawn", cb);
}

const _undefined = undefined;
export { _undefined as undefined };

export function bufferToReadStream(buf) {
  return Readable.from(buf);
}
