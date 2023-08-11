import Fuse from "fuse.js";

// The spdx-license-ids project is just a set of JSON files, which can't be
// imported directly into ESM without import assertions to guide TypeScript.
import { createRequire } from "module";
const require = createRequire(import.meta.url);
const identifiers = require("spdx-license-ids");
const deprecatedIdentifiers = require("spdx-license-ids/deprecated");

const allIdentifiers = identifiers.concat(deprecatedIdentifiers);
const fuse = new Fuse(allIdentifiers);

export const matchLicenseImpl = (identifier) =>
  fuse.search(identifier).map(({ item }) => item);
