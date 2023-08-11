import stream from "node:stream";
import { constants } from "node:buffer";

export const maxBufferLength = constants.MAX_LENGTH;

export function pipeline(source, destination, cb) {
  return stream.pipeline([source, destination], cb);
}
