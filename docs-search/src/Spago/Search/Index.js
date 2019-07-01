/* global exports */

exports.loadIndex_ = function (partId) {
  return function (url) {
    return function () {
      return new Promise(function(resolve, reject) {
        if (typeof window.spagoIndex[partId] === 'undefined') {

          var script = document.createElement('script');
          script.type = 'text/javascript';
          script.src = url;

          script.addEventListener('load', function () {
            if (typeof window.spagoIndex[partId] == 'undefined') {
              reject();
            } else {
              resolve(window.spagoIndex[partId]);
            }
          });

          script.addEventListener('error', reject);

          document.body.appendChild(script);
        } else {
          resolve(window.spagoIndex[partId]);
        }
      });
    };
  };
};
