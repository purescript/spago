export function timeNs(k) {
  const t1 = process.hrtime();
  k();
  const t2 = process.hrtime(t1);
  return t2[0] * 1.0e9 + t2[1];
}

export function gc() {
  global.gc && global.gc();
}

export function toFixed(n) {
  return n.toFixed(2);
}
