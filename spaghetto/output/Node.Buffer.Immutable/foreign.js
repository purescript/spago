/* global Buffer */
import { inspect } from "util";
export const showImpl = inspect;

export function eqImpl(a) {
  return b => {
    return a.equals(b);
  };
}

export function compareImpl(a) {
  return b => {
    return a.compare(b);
  };
}

export function create(size) {
  return Buffer.alloc(size);
}

export function fromArray(octets) {
  return Buffer.from(octets);
}

export function size(buff) {
  return buff.length;
}

export function toArray(buff) {
  var json = buff.toJSON();
  return json.data || json;
}

export function toArrayBuffer(buff) {
  return buff.buffer.slice(buff.byteOffset, buff.byteOffset + buff.byteLength);
}

export function fromArrayBuffer(ab) {
  return Buffer.from(ab);
}

export function fromStringImpl(str) {
  return encoding => {
    return Buffer.from(str, encoding);
  };
}

export function readImpl(ty) {
  return offset => {
    return buf => {
      return buf["read" + ty](offset);
    };
  };
}

export function readStringImpl(enc) {
  return start => {
    return end => {
      return buff => {
        return buff.toString(enc, start, end);
      };
    };
  };
}

export function getAtOffsetImpl(just) {
  return nothing => {
    return offset => {
      return buff => {
        var octet = buff[offset];
        return octet == null ? nothing : just(octet);
      };
    };
  };
}

export function toStringImpl(enc) {
  return buff => {
    return buff.toString(enc);
  };
}

export function slice(start) {
  return end => {
    return buff => {
      return buff.slice(start, end);
    };
  };
}

export function concat(buffs) {
  return Buffer.concat(buffs);
}

export function concatToLength(buffs) {
  return totalLength => {
    return Buffer.concat(buffs, totalLength);
  };
}
