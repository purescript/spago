export function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options) {
  return function (errback, callback) {
    var xhr = platformSpecificDriver.newXHR();
    var fixedUrl = platformSpecificDriver.fixupUrl(options.url, xhr);
    xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
    if (options.headers) {
      try {
        // eslint-disable-next-line no-eq-null,eqeqeq
        for (var i = 0, header; (header = options.headers[i]) != null; i++) {
          xhr.setRequestHeader(header.field, header.value);
        }
      } catch (e) {
        errback(e);
      }
    }
    var onerror = function (msgIdent) {
      return function () {
        errback(new Error(msgIdent));
      };
    };
    xhr.onerror = onerror(requestFailedMessageIdent);
    xhr.ontimeout = onerror(timeoutErrorMessageIdent);
    xhr.onload = function () {
      callback({
        status: xhr.status,
        statusText: xhr.statusText,
        headers: xhr.getAllResponseHeaders().split("\r\n")
          .filter(function (header) {
            return header.length > 0;
          })
          .map(function (header) {
            var i = header.indexOf(":");
            return mkHeader(header.substring(0, i))(header.substring(i + 2));
          }),
        body: xhr.response
      });
    };
    xhr.responseType = options.responseType;
    xhr.withCredentials = options.withCredentials;
    xhr.timeout = options.timeout;
    xhr.send(options.content);

    return function (error, cancelErrback, cancelCallback) {
      try {
        xhr.abort();
      } catch (e) {
        return cancelErrback(e);
      }
      return cancelCallback();
    };
  };
}
