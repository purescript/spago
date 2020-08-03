/* global exports */

exports.loadFromScript = function (globalIdentifier) {
  return function (url) {
    return function () {
      return new Promise(function (resolve, reject) {
        if (typeof window[globalIdentifier] === 'undefined') {
          var script = document.createElement('script');
          script.type = 'text/javascript';
          script.src = url;
          script.addEventListener('load', function () {
            if (typeof window[globalIdentifier] === 'undefined') {
              reject(new Error("Couldn't load package index."));
            } else {
              resolve(window[globalIdentifier]);
            }
          });
          script.addEventListener('error', reject);
          document.body.appendChild(script);
        } else {
          resolve(window[globalIdentifier]);
        }
      });
    };
  };
};
