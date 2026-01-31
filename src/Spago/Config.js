import Yaml from "yaml";

const getOrElse = (node, key, fallback) => {
  if (!node.has(key)) {
    node.set(key, fallback);
  }
  return node.get(key);
}

export function addPackagesToConfigImpl(doc, isTest, newPkgs) {
  const pkg = doc.get("package");

  const deps = (() => {
    if (isTest) {
      const test = getOrElse(pkg, "test", doc.createNode({ main: "Test.Main", dependencies: [] }));
      return getOrElse(test, "dependencies", doc.createNode([]));
    } else {
      return getOrElse(pkg, "dependencies", doc.createNode([]))
    }
  })();

  // Stringify this collection as
  //  - dep1
  //  - dep2
  // rather than
  //  [ dep1, dep2 ]
  deps.flow = false;

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

export function removePackagesFromConfigImpl(doc, isTest, shouldRemove) {
  const pkg = doc.get("package");

  const deps = isTest ? pkg.get("test").get("dependencies") : pkg.get("dependencies");
  let newItems = [];
  for (const el of deps.items) {
    if (
      (Yaml.isScalar(el) && shouldRemove(el.value)) ||
      (Yaml.isMap(el) && shouldRemove(el.items[0].key))
    ) {
      continue;
    }
    newItems.push(el);
  }
  newItems.sort();
  deps.items = newItems;
}

// Helper to update dependency items with ranges from a map.
//
// Dependencies in spago.yaml can be either:
//   - A scalar (bare package name): `- prelude`
//   - A map (name + range/version): `- prelude: ">=6.0.0 <7.0.0"`
//
// This function updates dependencies with ranges from rangesMap:
// - Scalars are converted to maps if a range is found
// - Existing maps are updated with the new range if one is provided
// - Dependencies not in rangesMap are preserved unchanged
function updateDepsWithRanges(doc, deps, rangesMap) {
  if (!deps || !deps.items) return;

  let newItems = [];
  for (const el of deps.items) {
    if (Yaml.isMap(el)) {
      // Already has a version range - check if we have an updated range
      const packageName = el.items[0].key.value;
      const range = rangesMap[packageName];
      if (range) {
        // Update with new range
        let newEl = new Map();
        newEl.set(packageName, range);
        newItems.push(doc.createNode(newEl));
      } else {
        // No update provided, keep unchanged
        newItems.push(el);
      }
    } else if (Yaml.isScalar(el)) {
      // Bare package name - look up range in the map
      const range = rangesMap[el.value];
      if (range) {
        // Found a range, convert scalar to map format
        let newEl = new Map();
        newEl.set(el.value, range);
        newItems.push(doc.createNode(newEl));
      } else {
        // No range provided for this package, keep the original scalar.
        newItems.push(el);
      }
    }
  }

  newItems.sort();
  deps.items = newItems;
}

// Add version ranges to package.dependencies
export function addRangesToConfigImpl(doc, rangesMap) {
  const deps = doc.get("package").get("dependencies");
  updateDepsWithRanges(doc, deps, rangesMap);
}

// Add version ranges to package.test.dependencies.
export function addTestRangesToConfigImpl(doc, rangesMap) {
  const test = doc.get("package").get("test");
  // in the above variant we don't test for the existence of dependencies, because
  // they must always exist. We are not guaranteed the existence of the test section,
  // so we need an additional check here
  if (!test) return;

  const deps = test.get("dependencies");
  updateDepsWithRanges(doc, deps, rangesMap);
}

// Note: this function assumes a few things:
// - the `publish` section exists
// - the new element does not already exist in the list (it just appends it)
export function addOwnerImpl(doc, owner) {
  const publish = doc.get("package").get("publish");
  let owners = getOrElse(publish, "owners", doc.createNode([]));
  owners.items.push(doc.createNode(owner));
}

export function setPackageSetVersionInConfigImpl(doc, version) {
  doc.setIn(["workspace", "packageSet", "registry"], version);
}

export function migrateV1ConfigImpl(doc) {
  // see here for more info about the visitor: https://eemeli.org/yaml/#finding-and-modifying-nodes
  let hasChanged = false;
  Yaml.visit(doc, {
    Pair(_idx, pair, _path) {
      // A previous version of the config used underscores in the config keys,
      // but we standardised on camelCase instead.
      // So here we crawl through the keys of each YamlMap and convert all the keys
      // to camelCase.
      if (pair.key && Yaml.isScalar(pair.key)) {
        pair.key.value = pair.key.value.replaceAll(/_./g, function (match) {
          hasChanged = true;
          return match.charAt(1).toUpperCase();
        });
      }
    }
  });
  if (hasChanged) {
    return doc;
  }
}

export function addPublishLocationToConfigImpl(doc, location) {
  doc.setIn(["package", "publish", "location"], location);
}
