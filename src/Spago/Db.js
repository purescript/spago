import Database from 'better-sqlite3';

export const connectImpl = (path, logger) => {
  logger('Connecting to database at ' + path);
  let db = new Database(path, {
    fileMustExist: false,
    // verbose: logger,
  });
  db.pragma('journal_mode = WAL');
  db.pragma('foreign_keys = ON');

  db.prepare(
    `CREATE TABLE IF NOT EXISTS package_sets
    ( version TEXT PRIMARY KEY NOT NULL
    , compiler TEXT NOT NULL
    , date TEXT NOT NULL
    )`
  ).run();
  db.prepare(
    `CREATE TABLE IF NOT EXISTS package_set_entries
    ( packageSetVersion TEXT NOT NULL
    , packageName TEXT NOT NULL
    , packageVersion TEXT NOT NULL
    , PRIMARY KEY (packageSetVersion, packageName, packageVersion)
    , FOREIGN KEY (packageSetVersion) REFERENCES package_sets(version)
    )`
  ).run();
  db.prepare(
    `CREATE TABLE IF NOT EXISTS last_git_pull
    ( key TEXT PRIMARY KEY NOT NULL
    , date TEXT NOT NULL
    )`
  ).run();
  db.prepare(
    `CREATE TABLE IF NOT EXISTS package_metadata
    ( name TEXT PRIMARY KEY NOT NULL
    , metadata TEXT NOT NULL
    , last_fetched TEXT NOT NULL
    )`
  ).run();
  // it would be lovely if we'd have a foreign key on package_metadata, but that would
  // require reading metadatas before manifests, which we can't always guarantee
  db.prepare(
    `CREATE TABLE IF NOT EXISTS package_manifests
    ( name TEXT NOT NULL
    , version TEXT NOT NULL
    , manifest TEXT NOT NULL
    , PRIMARY KEY (name, version)
    )`
  ).run();
  return db;
};

export const insertPackageSetImpl = (db, packageSet) => {
  db.prepare(
    'INSERT INTO package_sets (version, compiler, date) VALUES (@version, @compiler, @date)'
  ).run(packageSet);
};

export const insertPackageSetEntryImpl = (db, packageSetEntry) => {
  db.prepare(
    'INSERT INTO package_set_entries (packageSetVersion, packageName, packageVersion) VALUES (@packageSetVersion, @packageName, @packageVersion)'
  ).run(packageSetEntry);
};

export const selectLatestPackageSetByCompilerImpl = (db, compiler) => {
  const row = db
    .prepare(
      'SELECT * FROM package_sets WHERE compiler = ? ORDER BY date DESC LIMIT 1'
    )
    .get(compiler);
  return row;
};

export const selectPackageSetsImpl = (db) => {
  const row = db.prepare('SELECT * FROM package_sets ORDER BY date ASC').all();
  return row;
};

export const selectPackageSetEntriesBySetImpl = (db, packageSetVersion) => {
  const row = db
    .prepare('SELECT * FROM package_set_entries WHERE packageSetVersion = ?')
    .all(packageSetVersion);
  return row;
};

export const selectPackageSetEntriesByPackageImpl = (
  db,
  packageName,
  packageVersion
) => {
  const row = db
    .prepare(
      'SELECT * FROM package_set_entries WHERE packageName = ? AND packageVersion = ?'
    )
    .all(packageName, packageVersion);
  return row;
};

export const getLastPullImpl = (db, key) => {
  const row = db
    .prepare('SELECT * FROM last_git_pull WHERE key = ? LIMIT 1')
    .get(key);
  return row?.date;
};

export const updateLastPullImpl = (db, key, date) => {
  db.prepare(
    'INSERT OR REPLACE INTO last_git_pull (key, date) VALUES (@key, @date)'
  ).run({ key, date });
};

export const getManifestImpl = (db, name, version) => {
  const row = db
    .prepare(
      'SELECT * FROM package_manifests WHERE name = ? AND version = ? LIMIT 1'
    )
    .get(name, version);
  return row?.manifest;
};

export const insertManifestImpl = (db, name, version, manifest) => {
  db.prepare(
    'INSERT OR IGNORE INTO package_manifests (name, version, manifest) VALUES (@name, @version, @manifest)'
  ).run({ name, version, manifest });
};

export const removeManifestImpl = (db, name, version) => {
  db.prepare(
    'DELETE FROM package_manifests WHERE name = ? AND version = ?'
  ).run(name, version);
};

export const getMetadataImpl = (db, name) => {
  const row = db
    .prepare('SELECT * FROM package_metadata WHERE name = ? LIMIT 1')
    .get(name);
  return row;
};

export const getMetadatasImpl = (db, names) => {
  let query = 'SELECT * FROM package_metadata WHERE name IN (@names)';
  return db.prepare(query).all({ names: names.map((n) => `'${n}'`).join(',') });
};

export const insertMetadataImpl = (db, name, metadata, last_fetched) => {
  db.prepare(
    'INSERT OR REPLACE INTO package_metadata (name, metadata, last_fetched) VALUES (@name, @metadata, @last_fetched)'
  ).run({ name, metadata, last_fetched });
};
