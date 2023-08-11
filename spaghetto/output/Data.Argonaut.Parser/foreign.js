export function _jsonParser(fail, succ, s) {
  try {
    return succ(JSON.parse(s));
  }
  catch (e) {
    return fail(e.message);
  }
}
