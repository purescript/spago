import Database from "better-sqlite3";

export const connectImpl = (path, logger) => {
  logger("Connecting to database at " + path);
  let db = new Database(path, {
    fileMustExist: false,
    verbose: logger,
  });
  db.pragma("journal_mode = WAL");
  db.pragma("foreign_keys = ON");

  db.prepare(`CREATE TABLE IF NOT EXISTS package_sets
    ( version TEXT PRIMARY KEY NOT NULL
    , compiler TEXT NOT NULL
    , date TEXT NOT NULL
    )`).run();
  db.prepare(`CREATE TABLE IF NOT EXISTS package_set_entries
    ( packageSetVersion TEXT NOT NULL
    , packageName TEXT NOT NULL
    , packageVersion TEXT NOT NULL
    , PRIMARY KEY (packageSetVersion, packageName, packageVersion)
    , FOREIGN KEY (packageSetVersion) REFERENCES package_sets(version))`).run();
  // TODO: this is here as a placeholder, but not settled yet
  // db.prepare(`CREATE TABLE IF NOT EXISTS package_versions
  //   ( name TEXT NOT NULL
  //   , version TEXT NOT NULL
  //   , published INTEGER NOT NULL
  //   , date TEXT NOT NULL
  //   , manifest TEXT NOT NULL
  //   , location TEXT NOT NULL
  //   , PRIMARY KEY (name, version))`).run();
  return db;
};

export const insertPackageSetImpl = (db, packageSet) => {
  db.prepare(
    "INSERT INTO package_sets (version, compiler, date) VALUES (@version, @compiler, @date)"
  ).run(packageSet);
};

export const insertPackageVersionImpl = (db, packageVersion) => {
  db.prepare(
    "INSERT INTO package_versions (name, version, published, date, manifest, location) VALUES (@name, @version, @published, @date, @manifest, @location)"
  ).run(packageVersion);
}

export const insertPackageSetEntryImpl = (db, packageSetEntry) => {
  db.prepare(
    "INSERT INTO package_set_entries (packageSetVersion, packageName, packageVersion) VALUES (@packageSetVersion, @packageName, @packageVersion)"
  ).run(packageSetEntry);
}

export const selectLatestPackageSetByCompilerImpl = (db, compiler) => {
  const row = db
    .prepare("SELECT * FROM package_sets WHERE compiler = ? ORDER BY date DESC LIMIT 1")
    .get(compiler);
  return row;
}

export const selectPackageSetsImpl = (db) => {
  const row = db
    .prepare("SELECT * FROM package_sets ORDER BY date ASC")
    .all();
  return row;
}

export const selectPackageVersionImpl = (db, name, version) => {
  const row = db
    .prepare("SELECT * FROM package_versions WHERE name = ? AND version = ? LIMIT 1")
    .get(name, version);
  return row;
}

export const unpublishPackageVersionImpl = (db, name, version) => {
  db.prepare("UPDATE package_versions SET published = 0 WHERE name = ? AND version = ?").run(name, version);
}

export const selectPackageSetEntriesBySetImpl = (db, packageSetVersion) => {
  const row = db
    .prepare("SELECT * FROM package_set_entries WHERE packageSetVersion = ?")
    .all(packageSetVersion);
  return row;
}

export const selectPackageSetEntriesByPackageImpl = (db, packageName, packageVersion) => {
  const row = db
    .prepare("SELECT * FROM package_set_entries WHERE packageName = ? AND packageVersion = ?")
    .all(packageName, packageVersion);
  return row;
}
