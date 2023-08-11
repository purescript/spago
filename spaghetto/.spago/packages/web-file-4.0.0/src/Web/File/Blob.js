export function typeImpl(blob) { return blob.type; }

export function blobImpl(args) {
  return function (mediaType) {
    return new Blob(args, {type: mediaType});
  };
}

export function size(blob) { return blob.size; }

export function slice(contentType) {
  return function (start) {
    return function (end) {
      return function (blob) {
        return blob.slice(start, end, contentType);
      };
    };
  };
}
