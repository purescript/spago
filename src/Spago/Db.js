import { DatabaseSync } from "node:sqlite";
import fs from "node:fs";
import path from "node:path";

export const connectImpl = (databasePath, logger) => {
  logger("Connecting to database at " + databasePath);

  // Ensure directory exists
  const dir = path.dirname(databasePath);
  fs.mkdirSync(dir, { recursive: true });

  const db = new DatabaseSync(databasePath, {
    enableForeignKeyConstraints: true,
    timeout: 5000, // Wait up to 5s if database is locked
  });

  // WAL journal mode is persistent in the DB file header (bytes 18-19), so
  // once set it sticks across connections and reopens. We skip the PRAGMA when
  // it's already set to avoid hitting winTruncate on the wal-index (.shm),
  // which races between concurrent spago processes on Windows and surfaces as
  // SQLITE_IOERR_TRUNCATE (errcode 1546).
  //
  // When two fresh processes race the initial set, the loser's exec throws,
  // but the winner has already written WAL to the header — so we only re-throw
  // if WAL didn't actually end up enabled (i.e. the error wasn't the benign
  // race we expect).
  //
  // See:
  //   https://sqlite.org/pragma.html (journal_mode persistence)
  //   https://sqlite.org/fileformat.html (header bytes 18-19 = WAL marker)
  const inWal = () => db.prepare("PRAGMA journal_mode").get()?.journal_mode === "wal";
  if (!inWal()) {
    try { db.exec("PRAGMA journal_mode = WAL"); }
    catch (e) { if (!inWal()) throw e; }
  }

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
    , FOREIGN KEY (packageSetVersion) REFERENCES package_sets(version)
    )`).run();
  db.prepare(`CREATE TABLE IF NOT EXISTS last_git_pull
    ( key TEXT PRIMARY KEY NOT NULL
    , date TEXT NOT NULL
    )`).run();
  db.prepare(`CREATE TABLE IF NOT EXISTS package_metadata
    ( name TEXT PRIMARY KEY NOT NULL
    , metadata TEXT NOT NULL
    , last_fetched TEXT NOT NULL
    )`).run();
  // it would be lovely if we'd have a foreign key on package_metadata, but that would
  // require reading metadata before manifests, which we can't always guarantee
  db.prepare(`CREATE TABLE IF NOT EXISTS package_manifests
    ( name TEXT NOT NULL
    , version TEXT NOT NULL
    , manifest TEXT NOT NULL
    , PRIMARY KEY (name, version)
    )`).run();
  return db;
}

export const insertPackageSetImpl = (db, packageSet) => {
  db.prepare(
    "INSERT OR IGNORE INTO package_sets (version, compiler, date) VALUES (@version, @compiler, @date)"
  ).run(packageSet);
}

export const insertPackageSetEntryImpl = (db, packageSetEntry) => {
  db.prepare(
    "INSERT OR IGNORE INTO package_set_entries (packageSetVersion, packageName, packageVersion) VALUES (@packageSetVersion, @packageName, @packageVersion)"
  ).run(packageSetEntry);
}

export const withTransactionImpl = (db, action) => {
  db.exec("BEGIN IMMEDIATE");
  try {
    action();
    db.exec("COMMIT");
  } catch (e) {
    db.exec("ROLLBACK");
    throw e;
  }
}

export const selectLatestPackageSetByCompilerImpl = (db, compiler) => {
  const row = db
    .prepare("SELECT * FROM package_sets WHERE compiler = @compiler ORDER BY date DESC LIMIT 1")
    .get({ compiler });
  return row;
}

export const selectPackageSetsImpl = (db) => {
  const row = db
    .prepare("SELECT * FROM package_sets ORDER BY date ASC")
    .all();
  return row;
}

export const selectPackageSetEntriesBySetImpl = (db, packageSetVersion) => {
  const row = db
    .prepare("SELECT * FROM package_set_entries WHERE packageSetVersion = @packageSetVersion")
    .all({ packageSetVersion });
  return row;
}

export const selectPackageSetEntriesByPackageImpl = (db, packageName, packageVersion) => {
  const row = db
    .prepare("SELECT * FROM package_set_entries WHERE packageName = @packageName AND packageVersion = @packageVersion")
    .all({ packageName, packageVersion });
  return row;
}

export const getLastPullImpl = (db, key) => {
  const row = db
    .prepare("SELECT * FROM last_git_pull WHERE key = @key LIMIT 1")
    .get({ key });
  return row?.date;
}

export const updateLastPullImpl = (db, key, date) => {
  db.prepare("INSERT OR REPLACE INTO last_git_pull (key, date) VALUES (@key, @date)").run({ key, date });
}

export const getManifestImpl = (db, name, version) => {
  const row = db
    .prepare("SELECT * FROM package_manifests WHERE name = @name AND version = @version LIMIT 1")
    .get({ name, version });
  return row?.manifest;
}

export const insertManifestImpl = (db, name, version, manifest) => {
  db.prepare("INSERT OR IGNORE INTO package_manifests (name, version, manifest) VALUES (@name, @version, @manifest)").run({ name, version, manifest });
}

export const removeManifestImpl = (db, name, version) => {
  db.prepare("DELETE FROM package_manifests WHERE name = @name AND version = @version").run({ name, version });
}

export const insertMetadataImpl = (db, name, metadata, last_fetched) => {
  db.prepare("INSERT OR REPLACE INTO package_metadata (name, metadata, last_fetched) VALUES (@name, @metadata, @last_fetched)").run({ name, metadata, last_fetched });
}

export const getMetadataForPackagesImpl = (db, names) => {
  // There can be a lot of package names here, potentially hitting the max number of sqlite parameters, so we use json to bypass this
  const query = db.prepare("SELECT * FROM package_metadata WHERE name IN (SELECT value FROM json_each(@names));");
  return query.all({ names: JSON.stringify(names) });
}
