import stream from "node:stream";

export const buildCustomErrorImpl = (msg, obj) => 
  Object.assign(new Error(msg), obj);

export function newPassThroughStream() {
  return new stream.PassThrough({ objectMode: false });
}
