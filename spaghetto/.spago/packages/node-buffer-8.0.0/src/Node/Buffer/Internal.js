/* global Buffer */
export function copyAll(a) {
  return () => {
    return Buffer.from(a);
  };
}

export function writeInternal(ty) {
  return value => {
    return offset => {
      return buf => {
        return () => {
          buf["write" + ty](value, offset);
        };
      };
    };
  };
}

export function writeStringInternal(encoding) {
  return offset => {
    return length => {
      return value => {
        return buff => {
          return () => {
            return buff.write(value, offset, length, encoding);
          };
        };
      };
    };
  };
}

export function setAtOffset(value) {
  return offset => {
    return buff => {
      return () => {
        buff[offset] = value;
      };
    };
  };
}

export function copy(srcStart) {
  return srcEnd => {
    return src => {
      return targStart => {
        return targ => {
          return () => {
            return src.copy(targ, targStart, srcStart, srcEnd);
          };
        };
      };
    };
  };
}

export function fill(octet) {
  return start => {
    return end => {
      return buf => {
        return () => {
          buf.fill(octet, start, end);
        };
      };
    };
  };
}
