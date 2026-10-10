import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

test("database works without SQLite warnings and preserves unrelated warnings", () => {
  const directory = mkdtempSync(join(tmpdir(), "spago-db-test-"));
  try {
    const moduleUrl = new URL("../../src/Spago/Db.js", import.meta.url).href;
    const { NODE_OPTIONS, NODE_NO_WARNINGS, ...env } = process.env;
    const result = spawnSync(process.execPath, ["--input-type=module", "-e", `
      import { connectImpl, insertManifestImpl, getManifestImpl } from ${JSON.stringify(moduleUrl)};
      const db = connectImpl(${JSON.stringify(join(directory, "cache.sqlite"))}, () => {});
      try {
        insertManifestImpl(db, "example", "1.2.3", "saved manifest");
        console.log(getManifestImpl(db, "example", "1.2.3"));
      } finally {
        db.close();
      }
      process.emitWarning("unrelated experimental warning", "ExperimentalWarning");
      process.emitWarning("ordinary warning");
    `], { encoding: "utf8", env });
    assert.equal(result.status, 0, result.stderr);
    assert.equal(result.stdout.trim(), "saved manifest");
    assert.doesNotMatch(result.stderr, /SQLite is an experimental feature/);
    assert.match(result.stderr, /ExperimentalWarning: unrelated experimental warning/);
    assert.match(result.stderr, /Warning: ordinary warning/);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
