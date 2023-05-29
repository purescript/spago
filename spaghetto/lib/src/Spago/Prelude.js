import url from 'node:url';

export const parseUrlImpl = (onError, onSuccess, rawUrl) => {
  try {
    return onSuccess(new url.URL(rawUrl));
  } catch (err) {
    return onError(err);
  }
};

export const unsafeLog = (a) => () => {
  console.dir(a, { showHidden: false, depth: null, colors: true, maxArrayLength: null });
}
