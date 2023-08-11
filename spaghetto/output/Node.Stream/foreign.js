const _undefined = undefined;
export { _undefined as undefined };

export function setEncodingImpl(s) {
  return enc => () => {
    s.setEncoding(enc);
  };
}

export function readChunkImpl(Left) {
  return Right => chunk => {
    if (chunk instanceof Buffer) {
      return Right(chunk);
    } else if (typeof chunk === "string") {
      return Left(chunk);
    } else {
      throw new Error(
        "Node.Stream.readChunkImpl: Unrecognised " +
          "chunk type; expected String or Buffer, got: " +
          chunk
      );
    }
  };
}

export function onDataEitherImpl(readChunk) {
  return r => f => () => {
    r.on("data", data => {
      f(readChunk(data))();
    });
  };
}

export function onEnd(s) {
  return f => () => {
    s.on("end", f);
  };
}

export function onFinish(s) {
  return f => () => {
    s.on("finish", f);
  };
}

export function onReadable(s) {
  return f => () => {
    s.on("readable", f);
  };
}

export function onError(s) {
  return f => () => {
    s.on("error", e => {
      f(e)();
    });
  };
}

export function onClose(s) {
  return f => () => {
    s.on("close", f);
  };
}

export function resume(s) {
  return () => {
    s.resume();
  };
}

export function pause(s) {
  return () => {
    s.pause();
  };
}

export function isPaused(s) {
  return () => s.isPaused();
}

export function pipe(r) {
  return w => () => r.pipe(w);
}

export function unpipe(r) {
  return w => () => r.unpipe(w);
}

export function unpipeAll(r) {
  return () => r.unpipe();
}

export function readImpl(readChunk) {
  return Nothing => Just => r => s => () => {
    const v = r.read(s);
    if (v === null) {
      return Nothing;
    } else {
      return Just(readChunk(v));
    }
  };
}

export function writeImpl(w) {
  return chunk => done => () => w.write(chunk, null, done);
}

export function writeStringImpl(w) {
  return enc => s => done => () => w.write(s, enc, done);
}

export function cork(w) {
  return () => w.cork();
}

export function uncork(w) {
  return () => w.uncork();
}

export function setDefaultEncodingImpl(w) {
  return enc => () => {
    w.setDefaultEncoding(enc);
  };
}

export function endImpl(w) {
  return done => () => {
    w.end(null, null, done);
  };
}

export function destroy(strm) {
  return () => {
    strm.destroy(null);
  };
}

export function destroyWithError(strm) {
  return e => () => {
    strm.destroy(e);
  };
}
