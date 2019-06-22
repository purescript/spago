/* global exports */

exports.loadDeclarations_ = function (url) {
  return function () {
    return new Promise(function(resolve, reject) {
      var script = document.createElement('script');
      script.type = 'text/javascript';
      script.src = url;
      script.addEventListener('load', function () {
        resolve(window.spagoSearchIndex);
      });
      script.addEventListener('error', reject);
    });
  };
};
