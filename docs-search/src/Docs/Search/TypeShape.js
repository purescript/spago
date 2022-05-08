/* global exports */

export function hash (string) {
  var hash = Math.floor(Number.MAX_SAFE_INTEGER / 2);
  if (string.length == 0) {
    return hash;
  }
  for (var i = 0; i < string.length; i++) {
    var char = string.charCodeAt(i);
    hash = ((hash<<5)-hash)+char;
    hash = hash & hash; // Convert to 32bit integer
  }
  return hash;
};
