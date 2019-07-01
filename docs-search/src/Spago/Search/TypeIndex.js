/* global exports */

exports.lookup_ = function (shape) {
  return function (url) {
    return function () {
      return new Promise(function (resolve, reject) {
        if (typeof window.spagoTypeIndex[shape] === 'undefined') {
          var script = document.createElement('script');
          script.type = 'text/javascript';
          script.src = url;
          script.addEventListener('load', function () {
            if (typeof window.spagoTypeIndex[shape] === 'undefined') {
              reject(new Error("Couldn't load index for type shape " + shape));
            } else {
              resolve(window.spagoTypeIndex[shape]);
            }
          });
          script.addEventListener('error', reject);
          document.body.appendChild(script);
        } else {
          resolve(window.spagoTypeIndex[shape]);
        }
      });
    };
  };
};
