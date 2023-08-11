import path from "path";
export const normalize = path.normalize;

export function concat(segments) {
  return path.join.apply(this, segments);
}

export function resolve(from) {
  return to => () => path.resolve.apply(this, from.concat([to]));
}

export function relative(from) {
  return to => path.relative(from, to);
}

export function dirname(p) {
  return path.normalize(path.dirname(p));
}

export const basename = path.basename;

export function basenameWithoutExt(p) {
  return ext => path.basename(p, ext);
}

export const extname = path.extname;
export const sep = path.sep;
export const delimiter = path.delimiter;
export const parse = path.parse;
export const isAbsolute = path.isAbsolute;
