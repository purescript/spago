/* global exports */

exports.load = function (url) {
  return function () {
    return new Promise(function (resolve, reject) {
      if (typeof window.DocsSearchPackageIndex === 'undefined') {
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;
        script.addEventListener('load', function () {
          if (typeof window.DocsSearchPackageIndex === 'undefined') {
            reject(new Error("Couldn't load index for type shape " + shape));
          } else {
            resolve(window.DocsSearchPackageIndex);
          }
        });
        script.addEventListener('error', reject);
        document.body.appendChild(script);
      } else {
        resolve(window.DocsSearchPackageIndex);
      }
    });
  };
};
