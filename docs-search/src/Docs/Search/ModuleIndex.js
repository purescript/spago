/* global exports */

exports.load = function (url) {
  return function () {
    return new Promise(function (resolve, reject) {
      if (typeof window.DocsSearchModuleIndex === 'undefined') {
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;
        script.addEventListener('load', function () {
          if (typeof window.DocsSearchModuleIndex === 'undefined') {
            reject(new Error("Couldn't load module index"));
          } else {
            resolve(window.DocsSearchModuleIndex);
          }
        });
        script.addEventListener('error', reject);
        document.body.appendChild(script);
      } else {
        resolve(window.DocsSearchModuleIndex);
      }
    });
  };
};
