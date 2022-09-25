export function updatePackageSetHashInConfigImpl(doc, sha) {
  doc.get("workspace").get("set").set("hash", sha);
}
