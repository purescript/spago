import semver from "semver";

export const parseRangeImpl = (rangeString) => {
  // `validRange` cleans the input string (in loose mode) and converts it into a range
  // using only comparative operators (no ^, ~, -, or other range operators), and then
  // checks whether the result is a valid SemVer range. It then returns `null` if the
  // range is not valid, and the converted range if it is.
  //
  // For example:
  //
  // > validRange("^1.0.0")
  // ">=1.0.0 <2.0.0"
  //
  // In loose mode `validRange` will also fix common errors like including a 'v' prefix
  // or extra spaces:
  //
  // > validRange(" = v 2.1.5-foo")
  // "2.1.5-foo"
  //
  // We set these options because we should only be using this function to clean up ranges,
  // not to validate them.
  return semver.validRange(rangeString, {
    loose: true,
    includePrerelease: false,
  });
};
