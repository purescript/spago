// A helper which transforms the result of encodeURIComponent to be compliant
// with RFC3986, as described in the MDN documentation here:
//
// https://web.archive.org/web/20201206001047/https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
function encodeURIComponent_to_RFC3986(input) {
  return input.replace(/[!'()*]/g, function (c) {
    return "%" + c.charCodeAt(0).toString(16);
  });
}

// A helper which transforms the result of encodeURI to be compliant
// with RFC3986, as described in the MDN documentation here:
//
// https://web.archive.org/web/20210117175449/https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI#encoding_for_ipv6
function encodeURI_to_RFC3986(input) {
  return input.replace(/%5B/g, "[").replace(/%5D/g, "]");
}

export function _encodeURIComponent(fail, succeed, input) {
  try {
    return succeed(encodeURIComponent_to_RFC3986(encodeURIComponent(input)));
  } catch (err) {
    return fail(err);
  }
}

export function _encodeFormURLComponent(fail, succeed, input) {
  try {
    return succeed(encodeURIComponent_to_RFC3986(encodeURIComponent(input)).replace(/%20/g, "+"));
  } catch (err) {
    return fail(err);
  }
}

export function _decodeURIComponent(fail, succeed, input) {
  try {
    return succeed(decodeURIComponent(input));
  } catch (err) {
    return fail(err);
  }
}

export function _decodeFormURLComponent(fail, succeed, input) {
  return _decodeURIComponent(fail, succeed, input.replace(/\+/g, " "));
}

export function _encodeURI(fail, succeed, input) {
  try {
    return succeed(encodeURI_to_RFC3986(encodeURI(input)));
  } catch (err) {
    return fail(err);
  }
}

export function _decodeURI(fail, succeed, input) {
  try {
    return succeed(decodeURI(input));
  } catch (err) {
    return fail(err);
  }
}
