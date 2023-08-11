export function onceEnd(source, removeFn) {
  source.once("end", removeFn);
}

export function onceError(source, output) {
  source.once("error", output.emit.bind(output, "error"));
}

export function pipeImpl(source, output, options) {
  source.pipe(output, options);
}

export function readable(stream) {
  return stream.readable;
}
