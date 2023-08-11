export { inspect as showStatsObj } from "util";

export function statsMethod(m, s) {
  return s[m]();
}
