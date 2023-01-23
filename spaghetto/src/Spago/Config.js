import Yaml from "yaml";

export function updatePackageSetHashInConfigImpl(doc, sha) {
  doc.get("workspace").get("package_set").set("hash", sha);
}

export function addPackagesToConfigImpl(doc, newPkgs) {
  const deps = doc.get("package").get("dependencies");

  // Gather all deps, old and new, in a new set
  let depsSet = new Set(deps.toJSON());
  for (const pkg of newPkgs) {
    depsSet.add(pkg);
  }

  // now we go through the items - if an item is in the set we delete the name
  // from the set and add the package to a new list.
  // If it's not in the set then we do nothing and just discard it, as it's a duplicate
  let newItems = [];
  for (const el of deps.items) {
    if (Yaml.isScalar(el) && depsSet.has(el.value)) {
      depsSet.delete(el.value);
      newItems.push(el);
    }
    // If it's not a scalar then we have a version range, and we are dealing with a map
    if (Yaml.isMap(el) && depsSet.has(el.items[0].key)) {
      depsSet.delete(el.value);
      newItems.push(el);
    }
  }
  // any remaining values in the set are the new packages. We add them too
  for (const newPkg of depsSet) {
    newItems.push(doc.createNode(newPkg));
  }

  newItems.sort();
  deps.items = newItems;
}
