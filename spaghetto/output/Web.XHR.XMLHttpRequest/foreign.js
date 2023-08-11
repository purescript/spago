export function _xmlHttpRequest(respType) {
  var xhr = new XMLHttpRequest();
  xhr.responseType = respType;
  return xhr;
}

export function _abort(xhr) {
  xhr.abort();
}

export function _getAllResponseHeaders(xhr) {
  return xhr.getAllResponseHeaders();
}

export function _getResponseHeader(header, xhr) {
  return xhr.getResponseHeader(header);
}

export function _open(method, url, username, password, xhr) {
  xhr.open(method, url, true, username, password);
}

export function _overrideMimeType(mimeType, xhr) {
  xhr.overrideMimeType(mimeType);
}

export function _send(payload, xhr) {
  xhr.send(payload);
}

export function _setRequestHeader(header, value, xhr) {
  xhr.setRequestHeader(header, value);
}

export function _setProperty(prop, value, xhr) {
  xhr[prop] = value;
}

export function _getProperty(prop, xhr) {
  return xhr[prop];
}
