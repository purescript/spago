// Converts a number to a string of the nearest integer _without_ appending ".0"
// (like `show` for `Number`) or clamping to +/- 2 billion (like when working
// with `Int`). This is important for performance compared to other means of
// showing an integer potentially larger than +/- 2 billion.
export function showNumberAsInt(n) {
  return Math.round(n).toString();
}
