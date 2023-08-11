import events from "node:events";

const newImpl = function () {
  return new events.EventEmitter();
}
export { newImpl as new };

export function listenersLengthImpl(emitter, event) {
  return emitter.listeners(event).length;
}

export function setMaxListenersImpl(emitter, max) {
  emitter.setMaxListeners(max);
}

export function onImpl(emitter, eventName, cb) {
  emitter.on(eventName, cb);
  return cb;
}

export function offImpl(emitter, eventName, cb) {
  emitter.off(eventName, cb);
}

export function onceEventListener(emitter, eventName, cb) {
  emitter.once(eventName, cb);
}

export function emitImpl(emitter, eventName) {
  return function (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) {
    emitter.emit(eventName, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  };
}
const undefined_ = undefined
export { undefined_ as undefined }
