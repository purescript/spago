import { globSync } from "glob";
import path from "node:path";
import { fileURLToPath } from "node:url";

export function getDocsSearchAppPath() {
  const fileName = fileURLToPath(import.meta.url);
  const absoluteDir = path.dirname(fileName);
  const basename = path.basename(absoluteDir);
  
  // unbundled dev build
  if (basename == "Docs.Search.IndexBuilder") {
    return path.join(absoluteDir, "..", "..", "bin", "docs-search-app.js");
  }
  // bundled production build 
  if (basename === "bin") {
    return path.join(absoluteDir, "docs-search-app.js");
  }

  throw new Error("cannot locate halogen bundle path")
}

export function glob(pattern) {
  return function () {
    return globSync(pattern);
  };
}
