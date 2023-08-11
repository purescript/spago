import XHR from "xhr2";
import urllib from "url";

export const driver = {
  newXHR: function () {
    return new XHR();
  },
  fixupUrl: function (url, xhr) {
    if (xhr.nodejsBaseUrl === null) {
      var u = urllib.parse(url);
      u.protocol = u.protocol || "http:";
      u.hostname = u.hostname || "localhost";
      return urllib.format(u);
    } else {
      return url || "/";
    }
  },
};
