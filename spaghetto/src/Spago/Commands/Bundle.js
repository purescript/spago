import { build } from "esbuild";

/** @type {import('esbuild').BuildOptions} */
const defaultOptions = {
  minify: false,
  bundle: true,
  target: "node16",
  banner: {
    // https://github.com/evanw/esbuild/issues/1921
    js: `
import __module from 'module';
import __path from 'path';
import __url from 'url';
const require = __module.createRequire(import.meta.url);
const __filename = __url.fileURLToPath(import.meta.url);
const __dirname = __path.dirname(__filename);
`
  },
  platform: "node",
  format: "esm",
};

export const bundleImpl = (options) => () => {
  /** @type {import('esbuild').BuildOptions} */
  const opts = {
    ...defaultOptions,
    ...options,
  }

  build(opts)
    .then((res) => console.log(res))
    .catch((err) => {
      process.stderr.write(err.stderr);
      process.exit(1);
    });
};
