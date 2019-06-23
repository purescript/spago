/* global exports */

exports.loadDeclarations_ = function (url) {
  return function () {
    if (typeof window.spagoSearchIndex == 'undefined') {
      return new Promise(function(resolve, reject) {
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;
        script.addEventListener('load', function () {
          if (typeof window.spagoSearchIndex == 'undefined') {
            reject();
          } else {
            resolve(window.spagoSearchIndex);
          }
        });
        script.addEventListener('error', reject);
        document.body.appendChild(script);
      });
    } else {
      return Promise.resolve(window.spagoSearchIndex);
    }
  };
};
