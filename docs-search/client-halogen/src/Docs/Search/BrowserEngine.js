/* global exports */

export function loadIndex_(partId) {
  return function (url) {
    return function () {
      return new Promise(function (resolve, reject) {
        if (typeof window.DocsSearchIndex[partId] === "undefined") {
          var script = document.createElement("script");
          script.type = "text/javascript";
          script.src = url;

          script.addEventListener("load", function () {
            if (typeof window.DocsSearchIndex[partId] == "undefined") {
              reject();
            } else {
              resolve(window.DocsSearchIndex[partId]);
            }
          });

          script.addEventListener("error", reject);

          document.body.appendChild(script);
        } else {
          resolve(window.DocsSearchIndex[partId]);
        }
      });
    };
  };
}
